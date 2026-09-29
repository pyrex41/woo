(in-package :cl-user)
(defpackage woo
  (:nicknames :clack.handler.woo)
  (:use :cl
        :woo.specials
        :woo.signal)
  (:import-from :woo.response
                :*empty-chunk*
                :write-socket-string
                :write-socket-crlf
                :response-headers-bytes
                :write-response-headers
                :write-body-chunk
                :write-string-body-chunk
                :finish-response)
  (:import-from :woo.ev
                :*buffer-size*
                :*connection-timeout*
                :*evloop*
                :socket-remote-addr
                :socket-remote-port
                :with-sockaddr)
  #-woo-no-ssl
  (:import-from :woo.ssl
                :get-negotiated-protocol
                :*alpn-protocols*
                :configure-alpn)
  (:import-from :woo.websocket
                :websocket-p
                :compute-accept-key
                :setup-websocket
                :send-text-frame
                :send-binary-frame
                :send-ping
                :send-pong
                :send-close
                :write-websocket-upgrade-response
                :socket-upgraded-p
                :feed-websocket-data
                :take-pending-websocket-data)
  (:import-from :woo.http2.clack
                :make-http2-app-handler)
  (:import-from :woo.http2.constants
                :+connection-preface+
                :+connection-preface-length+)
  (:import-from :woo.util
                :integer-string-p)
  (:import-from :quri
                :uri
                :uri-path
                :uri-query)
  (:import-from :fast-http
                :make-http-request
                :make-parser
                :http-method
                :http-resource
                :http-headers
                :http-upgrade-p
                :http-chunked-p
                :http-content-length
                :http-major-version
                :http-minor-version
                :parsing-error
                :fast-http-error)
  (:import-from :smart-buffer
                :make-smart-buffer
                :write-to-buffer
                :finalize-buffer
                :buffer-on-memory-p
                :delete-stream-file
                :*default-disk-limit*
                :buffer-limit-exceeded)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-bytes-to-string
                :utf-8-byte-length)
  (:import-from :alexandria
                :hash-table-plist
                :copy-stream
                :if-let)
  (:export :run
           :stop
           :stop-gracefully
           :*buffer-size*
           :*connection-timeout*
           :*default-backlog-size*
           :*default-worker-num*
           ;; WebSocket exports
           :websocket-p
           :compute-accept-key
           :setup-websocket
           :send-text-frame
           :send-binary-frame
           :send-ping
           :send-pong
           :send-close
           :write-websocket-upgrade-response
           ;; SSL/ALPN exports
           #-woo-no-ssl :*alpn-protocols*
           #-woo-no-ssl :configure-alpn
           :http2-connection-preface-match
           :looks-like-http2-preface))
(in-package :woo)

(defvar *default-backlog-size* 128)
(defvar *default-worker-num* nil)

;; A threaded host must request shutdown on the event-loop thread.  Closing
;; only the listener and destroying that thread can bypass WITH-EVENT-LOOP's
;; socket cleanup.
(defstruct (stop-control (:constructor make-stop-control))
  listener thread evloop async cluster (command :stop))
(defvar *stop-controls* (make-hash-table :test #'eql))
(defvar *stop-controls-by-async* (make-hash-table :test #'eql))
(defvar *stop-controls-lock* (bt2:make-lock :name "woo-stop-controls"))

(cffi:defcallback stop-async-cb :void
    ((evloop :pointer) (async :pointer) (events :int))
  (declare (ignore events))
  (let ((control (bt2:with-lock-held (*stop-controls-lock*)
                   (gethash (cffi:pointer-address async)
                            *stop-controls-by-async*))))
    (when control
      (if (eq (stop-control-command control) :drain)
          (lev:ev-io-stop evloop (stop-control-listener control))
          (lev:ev-break evloop lev:+EVBREAK-ALL+)))))

(defun register-stop-control (listener cluster)
  (let* ((async (cffi:foreign-alloc '(:struct lev:ev-async)))
         (control (make-stop-control :listener listener
                                     :thread (bt2:current-thread)
                                     :evloop woo.ev:*evloop*
                                     :async async
                                     :cluster cluster)))
    (lev:ev-async-init async 'stop-async-cb)
    (lev:ev-async-start woo.ev:*evloop* async)
    (bt2:with-lock-held (*stop-controls-lock*)
      (setf (gethash (cffi:pointer-address listener) *stop-controls*) control
            (gethash (bt2:current-thread) *stop-controls*) control
            (gethash (cffi:pointer-address async) *stop-controls-by-async*) control))
    control))

(defun unregister-stop-control (control)
  (when control
    (bt2:with-lock-held (*stop-controls-lock*)
      (remhash (cffi:pointer-address (stop-control-listener control)) *stop-controls*)
      (remhash (stop-control-thread control) *stop-controls*)
      (remhash (cffi:pointer-address (stop-control-async control))
               *stop-controls-by-async*))
    (lev:ev-async-stop (stop-control-evloop control)
                       (stop-control-async control))
    (cffi:foreign-free (stop-control-async control))))

(defun stop-gracefully (server)
  "Request that SERVER's owning event loop stop and clean up its sockets.
SERVER may be the listener or the thread running WOO:RUN."
  (bt2:with-lock-held (*stop-controls-lock*)
    (let ((control (if (bt2:threadp server)
                       (gethash server *stop-controls*)
                       (gethash (cffi:pointer-address server) *stop-controls*))))
      (when control
        (setf (stop-control-command control) :stop)
        (lev:ev-async-send (stop-control-evloop control)
                           (stop-control-async control))
        t))))

(defun quiesce (thread)
  "Stop accepting connections without tearing down active responses."
  (bt2:with-lock-held (*stop-controls-lock*)
    (let ((control (gethash thread *stop-controls*)))
      (when control
        (setf (stop-control-command control) :drain)
        (lev:ev-async-send (stop-control-evloop control) (stop-control-async control))
        t))))

(defun http2-connection-preface-match (data start end)
  "Return T if DATA[START:END] contains a complete HTTP/2 connection preface."
  (and (>= (- end start) +connection-preface-length+)
       (loop for i from 0 below +connection-preface-length+
             always (= (aref data (+ start i))
                       (aref +connection-preface+ i)))))

(defun looks-like-http2-preface (data start end)
  "Classify bytes as :http2, :http1, or :need-more (h2c PRI preface)."
  (let ((n (- end start)))
    (cond
      ((zerop n) :need-more)
      ((>= n +connection-preface-length+)
       (if (http2-connection-preface-match data start end)
           :http2
           :http1))
      (t
       (if (loop for i from 0 below n
                 always (= (aref data (+ start i))
                           (aref +connection-preface+ i)))
           :need-more
           :http1)))))

(defun run (app &key (debug t)
                     (port 5000) (address "127.0.0.1")
                     listen ;; UNIX domain socket
                     (backlog *default-backlog-size*) fd
                     (worker-num *default-worker-num*)
                     ssl-key-file
                     ssl-cert-file
                     ssl-key-password on-ready on-connection (handle-signals t))
  (declare (ignorable ssl-key-password))
  (assert (and (integerp backlog)
               (plusp backlog)
               (<= backlog 65535)))
  (assert (or (and (integerp worker-num)
                   (< 0 worker-num))
              (null worker-num)))
  (when (stringp listen)
    (setf listen (pathname listen)))
  (check-type listen (or pathname null))

  (let ((*app* app)
        (*debug* debug)
        (*listener* nil)
        (ssl (or ssl-key-file ssl-cert-file))
        (http2-handler nil)
        (ssl-context nil))
    (labels ((ensure-http2-handler ()
               (unless http2-handler
                 (setf http2-handler (make-http2-app-handler *app*)))
               http2-handler)
             (close-listener ()
               ;; Listener cleanup runs on its owning event-loop thread,
               ;; before WITH-EVENT-LOOP destroys libev. Clear the binding
               ;; first so the outer unwind-protect remains idempotent.
               (let ((listener *listener*))
                 (setf *listener* nil)
                 (when listener
                   (wev:close-tcp-server listener))))
             (install-detected-protocol (socket use-http2)
               (if use-http2
                   (funcall (ensure-http2-handler) socket)
                   (setup-parser socket)))
             (start-socket (socket)
               ;; Do not query ALPN here: the TLS handshake has not run yet.
               ;; Handshake completes on the first successful ssl-read in tcp-read-cb.
               #-woo-no-ssl
               (when ssl
                 (woo.ssl:init-ssl-handle socket
                                          ssl-context
                                          ssl-cert-file
                                          ssl-key-file
                                          ssl-key-password))
               (when on-connection (funcall on-connection socket))
               (let ((pending (make-array 0 :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0))
                     (detected nil))
                 (setf (wev:socket-data socket)
                       (lambda (data &key (start 0) (end (length data)))
                         (if detected
                             (funcall (wev:socket-data socket) data :start start :end end)
                             (let ((n (- end start)))
                               (when (plusp n)
                                 (let ((old (length pending)))
                                   (adjust-array pending (+ old n) :fill-pointer (+ old n))
                                   (replace pending data :start1 old :start2 start :end2 end)))
                               (let ((use-h2 nil)
                                     (ready nil))
                                 #-woo-no-ssl
                                 (when ssl
                                   (let ((proto (get-negotiated-protocol socket)))
                                     (cond
                                       ((and proto (string= proto "h2"))
                                        (setf use-h2 t ready t))
                                       (proto
                                        (setf use-h2 nil ready t)))))
                                 (unless ready
                                   (ecase (looks-like-http2-preface pending 0 (length pending))
                                     (:http2 (setf use-h2 t ready t))
                                     (:http1 (setf use-h2 nil ready t))
                                     (:need-more nil)))
                                 (when ready
                                   (setf detected t)
                                   (install-detected-protocol socket use-h2)
                                   ;; Parsers declare simple octet vectors, so
                                   ;; replay a simple copy, not the adjustable buffer.
                                   (when (plusp (length pending))
                                     (let ((replay (coerce pending '(simple-array (unsigned-byte 8) (*)))))
                                       (funcall (wev:socket-data socket) replay
                                                :start 0 :end (length replay))))))))))
                 (woo.ev.tcp:start-listening-socket socket)))
             (start-multithread-server ()
               (unless (getf vom::*config* :woo.signal)
                 (vom:config :woo.signal :info))
               (let ((*cluster* (woo.worker:make-cluster worker-num #'start-socket))
                     (signal-watchers (and handle-signals (make-signal-watchers)))
                     (stop-control nil))
                 (wev:with-sockaddr
                   (unwind-protect
                          (wev:with-event-loop (:cleanup-fn
                                              (lambda ()
                                                (unwind-protect
                                                     (close-listener)
                                                  (unwind-protect
                                                       (stop-signal-watchers *evloop* signal-watchers)
                                                    (unregister-stop-control stop-control)))))
                          (when handle-signals (start-signal-watchers *evloop* signal-watchers))
                          (setq *listener*
                                (wev:tcp-server (or listen
                                                    (cons address port))
                                                #'read-cb
                                                :connect-cb
                                                (lambda (socket)
                                                  (woo.worker:add-job-to-cluster *cluster* socket))
                                                :backlog backlog
                                                :fd fd
                                                :sockopt wsock:+SO-REUSEADDR+))
                          (setf stop-control (register-stop-control *listener* *cluster*))
                          (when on-ready (funcall on-ready stop-control)))
                     (close-listener)
                     (woo.worker:stop-cluster *cluster*)))))
             (start-singlethread-server ()
               (let ((signal-watchers (and handle-signals (make-signal-watchers)))
                     (stop-control nil))
                 (wev:with-sockaddr
                   (unwind-protect
                          (wev:with-event-loop (:cleanup-fn
                                              (lambda ()
                                                (unwind-protect
                                                     (close-listener)
                                                  (unwind-protect
                                                       (stop-signal-watchers *evloop* signal-watchers)
                                                    (unregister-stop-control stop-control)))))
                          (when handle-signals (start-signal-watchers *evloop* signal-watchers))
                          (setq *listener*
                                (wev:tcp-server (or listen
                                                    (cons address port))
                                                #'read-cb
                                                :connect-cb #'start-socket
                                                :backlog backlog
                                                :fd fd
                                                :sockopt wsock:+SO-REUSEADDR+))
                          (setf stop-control (register-stop-control *listener* nil))
                          (when on-ready (funcall on-ready stop-control)))
                     (close-listener))))))
      ;; Context ownership begins with allocation, so setup failures must
      ;; run the same cleanup as a listener that started successfully.
      (unwind-protect
           (progn
             (when ssl
               #+woo-no-ssl
               (warn "SSL certificate is specified but Woo's SSL feature is off. Ignored.")
               #-woo-no-ssl
               (progn
                 (when ssl-key-file
                   (setf ssl-key-file
                         (uiop:native-namestring
                          (or (probe-file ssl-key-file)
                              (error "SSL private key file '~A' does not exist." ssl-key-file)))))
                 (when ssl-cert-file
                   (setf ssl-cert-file
                         (uiop:native-namestring
                          (or (probe-file ssl-cert-file)
                              (error "SSL certificate '~A' does not exist." ssl-cert-file)))))
                 (setf ssl-context
                       (woo.ssl:create-context ssl-cert-file ssl-key-file ssl-key-password))
                 ;; The callback storage lives until this context is freed.
                 (woo.ssl:configure-context-alpn ssl-context woo.ssl:*alpn-protocols*)))
             (if worker-num
                 (start-multithread-server)
                 (start-singlethread-server)))
        #-woo-no-ssl
        (when ssl-context (woo.ssl:free-context ssl-context))))))

(defun respond-and-close (socket status message)
  ;; The request may have unread body bytes.  Once this terminal response is
  ;; selected, stop application reads so they cannot race its TLS drain.
  (wev:stop-reading-for-close socket)
  (setf (wev:socket-data socket)
        (lambda (data &key start end)
          (declare (ignore data start end))))
  (let ((body (string-to-utf-8-bytes message)))
    (wev:with-async-writing (socket :write-cb #'wev:graceful-close-socket)
      (write-response-headers socket status
                              (list :connection "close"
                                    :content-length (length body)))
      (wev:write-socket-data socket body))))

(defun read-cb (socket data &key (start 0) (end (length data)))
  (let ((parser (wev:socket-data socket)))
    (block read
      (handler-bind (((or fast-http:cb-headers-complete fast-http:cb-body)
                      (lambda (condition)
                        (let ((cause (slot-value condition 'fast-http.error::error)))
                          (when (typep cause 'buffer-limit-exceeded)
                            (vom:error "~A" cause)
                            (respond-and-close socket 413
                                                "413 Request Entity Too Large")
                            (return-from read nil))))))
        (handler-case (funcall parser data :start start :end end)
          (woo.ev.condition:output-limit-exceeded () (wev:close-socket socket))
          (request-body-limit-exceeded ()
            (respond-and-close socket 413 "413 Request Entity Too Large"))
          (fast-http:parsing-error (e)
            (vom:error "HTTP parse error: ~A" e)
            (respond-and-close socket 400 "400 Bad Request")))))))

(define-condition request-body-limit-exceeded (fast-http:fast-http-error) ()
  (:report (lambda (condition stream) (declare (ignore condition))
             (write-string "Request body admission limit exceeded" stream))))
(define-condition woo-error (simple-error) ())
(define-condition invalid-http-version (woo-error) ())

(defun error-invalid-http-version (major minor)
  (error 'invalid-http-version
           :format-control "INVALID-HTTP-VERSION: major ~A minor ~A"
           :format-arguments (list major minor)))

(defun http-version-keyword (major minor)
  (unless (= major 1)
    (error-invalid-http-version major minor))
  (case minor
    (1 :HTTP/1.1)
    (0 :HTTP/1.0)
    (otherwise (error-invalid-http-version major minor))))

(declaim (inline crlf-match-step))
(defun crlf-match-step (match octet)
  "The length of the CR LF CR LF prefix ending at OCTET, given the length
   MATCH of the one before it."
  (declare (type (integer 0 3) match)
           (type (unsigned-byte 8) octet)
           (optimize (speed 3) (safety 0)))
  (cond ((= octet (if (evenp match) 13 10)) (1+ match))
        ((= octet 13) 1)
        (t 0)))

(declaim (inline field-prefix-p))
(defun field-prefix-p (data i end name)
  "True if DATA[I..] starts with NAME (lower case, ending in #\:) in any
   case, or with a prefix of it that the end of the read cuts off."
  (declare (type (simple-array (unsigned-byte 8) (*)) data name)
           (type fixnum i end)
           (optimize (speed 3) (safety 0)))
  ;; OR-ing #x20 folds ASCII upper case to lower; it leaves #\- and #\:.
  (loop for k of-type fixnum from 0 below (length name)
        do (cond ((>= (+ i k) end) (return t))
                 ((/= (logior (aref data (+ i k)) #x20) (aref name k)) (return nil)))
        finally (return t)))

(defconstant +field-upgrade+ 1
  "LINE-FIELD bit: an Upgrade field, the one fast-http flags.")
(defconstant +field-body+ 2
  "LINE-FIELD bit: a Content-Length or Transfer-Encoding field, the only
   ones that give fast-http a request body.")

(declaim (inline line-field))
(defun line-field (data i end)
  "LINE-FIELD bits for the header line starting at DATA[I]. A line the end
   of the read cuts off may be either."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum i end)
           (optimize (speed 3) (safety 0)))
  (if (>= i end)
      (logior +field-upgrade+ +field-body+)
      (macrolet ((octets (string)
                   (map '(simple-array (unsigned-byte 8) (*)) #'char-code string)))
        (case (logior (aref data i) #x20)
          (#.(char-code #\u)
           (if (field-prefix-p data i end (octets "upgrade:")) +field-upgrade+ 0))
          (#.(char-code #\c)
           (if (field-prefix-p data i end (octets "content-length:")) +field-body+ 0))
          (#.(char-code #\t)
           (if (field-prefix-p data i end (octets "transfer-encoding:")) +field-body+ 0))
          (otherwise 0)))))

(defun find-header-end (data start end match)
  "Scan DATA[START:END] for the CR LF CR LF that ends a header block. MATCH
   is the length of a prefix of it ending just before START (from the
   previous read). Values: the index after the CR LF CR LF, or END; the
   prefix length there (2 after a match: its CR LF can begin the next);
   whether it was found; and the LINE-FIELD bits of the lines scanned."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end)
           (type (integer 0 3) match)
           (optimize (speed 3) (safety 0)))
  (let ((i start)
        (fields 0))
    (declare (type fixnum i fields))
    (loop
      (cond
        ((and (zerop match) (< (+ i 3) end))
         ;; Fast path: find the next CR and look at what follows it.
         (loop while (and (< i end) (/= (aref data i) 13))
               do (incf i))
         (cond
           ((>= (+ i 3) end))           ; near the end: take the slow path
           ((/= (aref data (+ i 1)) 10)
            (incf i))
           ((and (= (aref data (+ i 2)) 13) (= (aref data (+ i 3)) 10))
            (return (values (+ i 4) 2 t fields)))
           (t
            (incf i 2)
            (setq fields (logior fields (line-field data i end))))))
        ((>= i end)
         (return (values end match nil fields)))
        (t
         ;; Octet by octet: a match carried over, or the last 3 octets.
         (let ((next (crlf-match-step match (aref data i))))
           (incf i)
           (case next
             (4 (return (values i 2 t fields)))
             (2 (setq fields (logior fields (line-field data i end)))))
           (setq match next)))))))

(defun crlf-match-after (data start end match)
  "The CR LF CR LF prefix length at END, having skipped DATA[START:END]
   unscanned with MATCH at START. It depends only on the last 3 octets."
  (declare (type (simple-array (unsigned-byte 8) (*)) data)
           (type fixnum start end)
           (type (integer 0 3) match)
           (optimize (speed 3) (safety 0)))
  (let ((m (if (>= (- end start) 3) 0 match)))
    (declare (type (integer 0 4) m))
    (loop for i of-type fixnum from (max start (- end 3)) below end
          do (setq m (crlf-match-step (if (= m 4) 2 m) (aref data i))))
    (if (= m 4) 2 m)))

(defun make-request-body-buffer (socket)
  (let ((limit (woo.ev.socket::socket-body-memory-limit socket)))
    (if limit
        (make-smart-buffer
         :memory-limit (min limit smart-buffer:*default-memory-limit*)
         :disk-limit limit)
        (make-smart-buffer))))

(defun setup-parser (socket)
  ;; A request with an Upgrade header may be followed, in the same read, by
  ;; octets of the new protocol: a WebSocket client need not wait for the
  ;; 101. fast-http stops at the end of such a request but does not say
  ;; where that is. So the reader scans request heads (never bodies) for
  ;; the CR LF CR LF that ends each and for the fields that matter: an
  ;; upgrade request is fed only up to the end of its head, or of its
  ;; Content-Length body. Bodiless requests before it go in the same piece;
  ;; a fixed-length body is cut at its end so the next request can be
  ;; scanned. Chunked bodies cannot be cut without parsing their trailers;
  ;; an upgrade pipelined behind one in the same read is still handled by
  ;; fast-http alone. Once the application has upgraded the
  ;; socket, or is still deciding (a delayed response), the rest of the read
  ;; and every later one go to FEED-WEBSOCKET-DATA, never to fast-http.
  (let ((http (make-http-request))
        (body-buffer (make-request-body-buffer socket))
        (body-streams nil)
        ;; The request being parsed carries an Upgrade header.
        (upgrade-request nil)
        ;; The piece being parsed ends at the end of an Upgrade request's
        ;; head, so a head completing in it completes at its end.
        (aligned nil)
        ;; LINE-FIELD bits of the head being scanned, so far.
        (head-fields 0)
        ;; A request with an Upgrade header has been answered.
        (upgrade-seen nil)
        ;; An upgrade request's delayed response has not been given yet.
        (response-pending nil)
        ;; Octets are being held for WebSocket while RESPONSE-PENDING.
        (holding nil)
        ;; Length of the CR LF CR LF prefix that ends the octets read.
        (crlf-match 0)
        ;; Remaining bytes in the current fixed-length body. This is kept
        ;; separately from FAST-HTTP's mutable content-length slot because the
        ;; reader may split one socket read across the body and next request.
        (body-remaining nil)
        parser
        reader
        (request-completed nil))
    (declare (type (integer 0 3) crlf-match)
             (type fixnum head-fields))
    (labels ((release-body (stream)
               (setf body-streams (delete stream body-streams :test #'eq))
               (unwind-protect
                    (ignore-errors (close stream))
                 (ignore-errors (delete-stream-file stream))))
             (cleanup-open-buffer (buffer)
               (unless (buffer-on-memory-p buffer)
                 (ignore-errors
                  (let ((stream (finalize-buffer buffer)))
                    (release-body stream)))))
             (next-split (data start end)
               ;; The end of the next piece to feed fast-http. Second value:
               ;; true if octets after a completed head go in unscanned.
               (declare (type (simple-array (unsigned-byte 8) (*)) data)
                        (type fixnum start end))
               (let ((state (fast-http.http:http-state http)))
                 (cond
                   ((or (= state fast-http.http:+state-first-line+)
                        (= state fast-http.http:+state-headers+))
                    ;; Scan heads while their requests have no body: a run of
                    ;; pipelined GETs is one piece, up to and including an
                    ;; upgrade request's head.
                    (let ((pos start))
                      (declare (type fixnum pos))
                      (loop
                        (multiple-value-bind (split match found fields)
                            (find-header-end data pos end crlf-match)
                          (declare (type fixnum split fields))
                          (setq crlf-match match
                                head-fields (logior head-fields fields))
                          (cond
                            ((not found)
                             (setq aligned nil)
                             (return end))
                            ((logtest head-fields +field-upgrade+)
                             ;; Stop at the end of this head: what follows
                             ;; may belong to the new protocol.
                             (setq head-fields 0
                                   aligned t)
                             (return split))
                            ((logtest head-fields +field-body+)
                             ;; Feed the head separately so fast-http tells
                             ;; us whether the body has a fixed length. Never
                             ;; scan the body for what looks like headers.
                             (setq head-fields 0
                                   aligned t)
                             (return split))
                            ((or (= split end) (woo.ev.socket::socket-body-admitter socket))
                             (setq head-fields 0
                                   aligned nil)
                             (return split))
                            (t
                             (setq head-fields 0
                                   pos split)))))))
                   (t
                    ;; A body is never scanned. Cut a fixed-length body at
                    ;; its end, then scan the next request's head.
                    (setq aligned nil)
                    (let* ((n body-remaining)
                           (split (if (and (= state fast-http.http:+state-body+)
                                           (typep n 'fixnum)
                                           (plusp n))
                                      (min end (+ start n))
                                      (if (woo.ev.socket::socket-body-admitter socket)
                                          (min end (1+ start)) end))))
                      (declare (type fixnum split))
                      (setq crlf-match (crlf-match-after data start split crlf-match))
                      (values split (and (= split end)
                                         (http-chunked-p http))))))))
             (hold-for-websocket (data start end)
               ;; From now on reads are buffered until SETUP-WEBSOCKET
               ;; installs its reader (or, if it already has, parsed by it).
               (when (eq (wev:socket-data socket) reader)
                 (setf (wev:socket-data socket)
                       (lambda (data &key (start 0) (end (length data)))
                         (feed-websocket-data socket data :start start :end end))))
               (setq holding (not (socket-upgraded-p socket)))
               (feed-websocket-data socket data :start start :end end))
             (resume-http ()
               ;; A delayed response declined the upgrade: the held octets
               ;; are HTTP after all.
               (when (and holding
                          (not (socket-upgraded-p socket))
                          (woo.ev.socket:socket-open-p socket))
                 (setq holding nil)
                 (let ((held (take-pending-websocket-data socket)))
                   (setf (wev:socket-data socket) reader)
                   (when held
                     (read-cb socket held)))))
             (watch-response (upgrading res)
               (if (and upgrading (functionp res))
                   (progn
                     (setq response-pending t)
                     (lambda (responder)
                       (funcall res
                                (lambda (clack-res)
                                  (setq response-pending nil)
                                  (prog1 (funcall responder clack-res)
                                    (resume-http))))))
                   res)))
      (setq parser
            (make-parser http
                         :first-line-callback
                         (lambda ()
                           ;; fast-http retains these fields between messages.
                           (setf (http-content-length http) nil
                                 (http-chunked-p http) nil
                                 body-remaining nil))
                         :header-callback
                         (lambda (headers)
                           (let ((content-length (or (http-content-length http)
                                                     (gethash "content-length" headers))))
                             (when (and (stringp content-length)
                                        (integer-string-p content-length))
                               (setf content-length (parse-integer content-length)))
                             (let ((limit (or (woo.ev.socket::socket-body-memory-limit socket)
                                              *default-disk-limit*)))
                               (when (and content-length (> content-length limit))
                                 (error 'buffer-limit-exceeded :limit limit)))
                             (setf body-remaining
                                   (and (not (http-chunked-p http)) content-length)))
                           (when (http-upgrade-p http)
                             (setq upgrade-request t)
                             ;; fast-http would take a Content-Length body for
                             ;; the new protocol's first octets. Let it read the
                             ;; body; NEXT-SPLIT stops the piece where it ends.
                             ;; Not when the rest of the piece is unknown (the
                             ;; head did not end it) or the body is chunked:
                             ;; fast-http then stops here, as it always did.
                             (when (and aligned (not (http-chunked-p http)))
                               (setf (http-upgrade-p http) nil))))
                         :body-callback
                         (lambda (data start end)
                           (declare (type (simple-array (unsigned-byte 8) (*)) data))
                           (when (and (woo.ev.socket::socket-body-admitter socket)
                                      (not (funcall (woo.ev.socket::socket-body-admitter socket) (- end start))))
                             (error 'request-body-limit-exceeded))
                           (if (smart-buffer::buffer-on-memory-p body-buffer)
                               (write-to-buffer body-buffer (subseq data start end) 0 (- end start))
                               (write-to-buffer body-buffer data start end)))
                         :finish-callback
                         (lambda ()
                           (setf request-completed t)
                           (let ((upgrading upgrade-request))
                             ;; fast-http never clears the flag itself.
                             (setq upgrade-request nil)
                             (when upgrading
                               (setq upgrade-seen t))
                             (setf (http-upgrade-p http) nil)
                             (flet ((main (env release)
                                      (let ((response
                                              (watch-response
                                               upgrading
                                               (if *debug*
                                                   (funcall *app* env)
                                                   (if-let (res (handler-case (funcall *app* env)
                                                                  (error (error)
                                                                    (vom:error (princ-to-string error))
                                                                    nil)))
                                                     res
                                                     '(500 nil nil))))))
                                        (handle-response http socket response env release
                                                         (eq (http-method http) :head))
                                        response)))
                               (block result
                                 (let ((raw-body (finalize-buffer body-buffer)))
                                   (setq body-buffer (make-request-body-buffer socket))
                                   (handler-bind
                                       ((error ;; handle errors inside woo
                                          (lambda (e)
                                            (unless *debug*
                                              (vom:crit (princ-to-string e))
                                              (return-from result (handle-response http socket '(500 nil nil)))))))
                                     (let ((env (nconc (list :raw-body raw-body :woo.response-handler nil)
                                                       (handle-request http socket))))
                                       (unless (getf env :woo.response-handler)
                                         (push raw-body body-streams))
                                       (let ((result (main env (lambda ()
                                                                 (unless (getf env :woo.response-handler)
                                                                   (release-body raw-body))))))
                                         (when (and (not (getf env :woo.response-handler))
                                                    (listp result))
                                           (release-body raw-body))))))))))))
      (setq reader
            (lambda (data &key (start 0) (end (length data)))
              (declare (type (simple-array (unsigned-byte 8) (*)) data)
                       (type fixnum start end))
              (loop
                (when (>= start end)
                  (return))
                (when (woo.ev.socket::socket-input-paused-p socket)
                  (when (woo.ev.socket::socket-input-holder socket)
                    (funcall (woo.ev.socket::socket-input-holder socket) data start end))
                  (return))
                ;; No HTTP parsing once the socket belongs to WebSocket.
                ;; Only an upgrade request's response can upgrade it
                ;; (WEBSOCKET-P requires the header), so the lookup is
                ;; skipped until one has been answered.
                (when (or response-pending
                          (and upgrade-seen (socket-upgraded-p socket)))
                  (return (hold-for-websocket data start end)))
                (multiple-value-bind (split unscanned) (next-split data start end)
                  (setf request-completed nil)
                  (let ((body-state (= (fast-http.http:http-state http)
                                       fast-http.http:+state-body+)))
                    (funcall parser data :start start :end split)
                    (when (and body-state (typep body-remaining 'fixnum))
                      (decf body-remaining (- split start))))
                  ;; Unlike fixed bodies, fast-http does not reset its state
                  ;; after a chunked message. The next piece is a fresh request.
                  (when (and request-completed (http-chunked-p http))
                    (setf (fast-http.http:http-state http) fast-http.http:+state-first-line+))
                  (setq start split)
                  ;; fast-http may have read part of a later head unseen;
                  ;; its fields are then unknown.
                  (when (and unscanned
                             (= (fast-http.http:http-state http)
                                fast-http.http:+state-headers+))
                    (setq head-fields (logior +field-upgrade+ +field-body+)))))))
      (push (lambda ()
              ;; Managed compatibility requests transfer body ownership to
              ;; the request lifecycle once the application starts. Their
              ;; connection close hook marks wire completion; releasing here
              ;; would race an application producer still reading the body.
              (unless (woo.ev.socket::socket-body-admitter socket)
                (dolist (stream (copy-list body-streams))
                  (release-body stream)))
              (cleanup-open-buffer body-buffer))
            (woo.ev.socket::socket-close-hooks socket))
      (setf (wev:socket-data socket) reader))))

(defun stop (server)
  (cond ((null server) nil)
        ((stop-gracefully server) t)
        ((bt2:threadp server) nil)
        (t (wev:close-tcp-server server))))


;;
;; Handling requests

(defun parse-host-header (host)
  (woo.http2.clack::split-authority host))

(defun handle-request (http socket)
  (let ((host (gethash "host" (http-headers http)))
        (headers (http-headers http))
        (uri (http-resource http)))
    (declare (type simple-string uri))

    (multiple-value-bind (scheme userinfo hostname port path query fragment)
        (quri:parse-uri uri)
      (declare (ignore scheme userinfo hostname port fragment))
      (multiple-value-bind (server-name server-port)
          (if (stringp host)
              (parse-host-header host)
              (values nil nil))
        (list :request-method (http-method http)
              :script-name ""
              :server-name server-name
              :server-port (or server-port 80)
              :server-protocol (http-version-keyword (http-major-version http) (http-minor-version http))
              :path-info (if (and (stringp path)
                                  (string/= path ""))
                             (quri:url-decode path :lenient t)
                             "/")
              :query-string query
              :url-scheme "http"
              :remote-addr (socket-remote-addr socket)
              :remote-port (socket-remote-port socket)
              :request-uri uri
              :clack.streaming t
              :clack.nonblocking t
              :clack.io socket
              :content-length (let ((content-length (gethash "content-length" headers)))
                                (etypecase content-length
                                  (string (if (integer-string-p content-length)
                                              (parse-integer content-length)
                                              (error "Invalid Content-Length header: ~S" content-length)))
                                  (integer content-length)
                                  (null nil)))
              :content-type (gethash "content-type" headers)
              :headers headers)))))


;;
;; Handling responses

(defun validate-response (response &optional head-p)
  "Validate a legacy response before any bytes are committed."
  (unless (and (listp response) (member (list-length response) '(2 3)))
    (error "Invalid Clack response shape"))
  (destructuring-bind (status headers &optional body) response
    (check-type status (integer 200 599))
    (unless (and (listp headers) (let ((n (list-length headers))) (and n (evenp n))))
      (error "Invalid Clack response headers"))
    (loop for (key value) on headers by #'cddr
          do (unless (and (typep key '(or string symbol))
                          (plusp (length (string key)))
                          (every (lambda (c) (and (< (char-code c) 128)
                                                 (or (alphanumericp c)
                                                     (find c "!#$%&'*+-.^_`|~"))))
                                 (string key))
                          (or (null value)
                              (every (lambda (c) (and (<= (char-code c) 255)
                                                     (or (char= c #\Tab) (>= (char-code c) 32))
                                                     (/= (char-code c) 127)))
                                     (princ-to-string value))))
               (error "Invalid Clack response header")))
    (let ((lengths (loop for (key value) on headers by #'cddr
                         when (and value (string-equal (string key) "content-length")) collect value))
          (encodings (loop for (key value) on headers by #'cddr
                           when (and value (string-equal (string key) "transfer-encoding")) collect value)))
      (when (or (> (length lengths) 1) (and lengths encodings))
        (error "Ambiguous response framing"))
      (when lengths
        (let ((text (princ-to-string (first lengths))))
          (unless (and (plusp (length text)) (every #'digit-char-p text))
            (error "Invalid response content length"))))
      (when (and encodings (or (> (length encodings) 1)
                               (not (string-equal (princ-to-string (first encodings)) "chunked"))))
        (error "Unsupported response transfer encoding")))
    (unless (or (null body) (pathnamep body) (stringp body)
                (typep body '(vector (unsigned-byte 8)))
                (and (listp body) (list-length body) (every #'stringp body)))
      (error "Invalid Clack response body"))
    (let ((normalized (loop for (key value) on headers by #'cddr
                            append (list (woo.response::canonical-header-name key) value))))
      (when (and (= (length response) 3) (not head-p)
                 (not (member status '(204 205 304))) (not (pathnamep body))
                 (getf normalized :content-length))
        (let ((expected (parse-integer (princ-to-string (getf normalized :content-length))))
              (actual (typecase body
                        (null 0)
                        (string (utf-8-byte-length body))
                        (list (loop for chunk in body sum (utf-8-byte-length chunk)))
                        (vector (length body)))))
          (unless (= expected actual) (error "Response content length mismatch"))))
      ;; Framing is generated by the body writer; don't emit duplicate TE.
      (remf normalized :transfer-encoding)
      (if (= (length response) 2) (list status normalized)
          (list status normalized (if (stringp body) (list body) body))))))

(defun legacy-response-failed (http socket generation)
  (when (wev:socket-open-p socket)
    (if (/= generation (woo.ev.socket::socket-response-generation socket))
        (wev:close-socket socket)
        (progn
          ;; The response may already have flushed before this callback is
          ;; installed. Ask the socket to drain immediately so both buffered
          ;; and already-flushed responses reach TLS close_notify.
          (handle-normal-response http socket '(500 (:connection "close") nil))
          (wev:graceful-close-socket socket)))))

(defun handle-response (http socket clack-res &optional env body-cleanup head-p)

  (when (getf env :woo.response-handler)
    (return-from handle-response
      (funcall (getf env :woo.response-handler) http socket clack-res)))
  ;; After a WebSocket upgrade the socket carries frames, not HTTP. Whatever
  ;; the app returned (NIL, which becomes a 500, or a framework's finalized
  ;; 200), writing it would corrupt the stream.
  (when (socket-upgraded-p socket)
    (return-from handle-response nil))
  (let ((generation (woo.ev.socket::socket-response-generation socket)))
    (handler-case
        (etypecase clack-res
          (list (handle-normal-response http socket (validate-response clack-res head-p) body-cleanup head-p))
          (function
           (funcall clack-res
                    (lambda (response)
                      (unless (socket-upgraded-p socket)
                        (handler-case
                            (handle-normal-response http socket (validate-response response head-p) body-cleanup head-p)
                          (error () (legacy-response-failed http socket generation))))))))
      (error () (legacy-response-failed http socket generation)))))


#+sbcl
(defun fd-file-size (fd)
  ;; FSTAT mutates its destination; workers must never share that object.
  (let ((stat (make-instance 'sb-posix:stat)))
    (sb-posix:fstat fd stat)
    (sb-posix:stat-size stat)))
#+ccl
(defun fd-file-size (fd)
  (multiple-value-bind (successp mode size)
      (ccl::%fstat fd)
    (declare (ignore mode))
    (unless successp
      (error "'fstat' failed"))
    size))
#+lispworks
(defun file-size (path)
  (sys:file-size path))
#-(or sbcl ccl lispworks)
(defun file-size (path)
  (with-open-file (in path)
    (file-length in)))

(defun make-streaming-writer (socket &optional body-cleanup head-p)
  (lambda (body &key (start 0 has-start) (end nil has-end) (close nil))
    (if (and body head-p)
        (when close
          (wev:with-async-writing (socket)
            (finish-response socket *empty-chunk*)
            (when body-cleanup (funcall body-cleanup))))
        (if body
        (wev:with-async-writing (socket :force-streaming t)
          (etypecase body
            (string
             (write-string-body-chunk socket
                                      (if (or has-start has-end)
                                          (subseq body start end)
                                          body)))
            (vector (write-body-chunk socket body
                                      :start start
                                      :end (or end (length body)))))
          (when close
            (finish-response socket *empty-chunk*)
            (when body-cleanup (funcall body-cleanup))))
        (when close
          (wev:with-async-writing (socket)
            (finish-response socket *empty-chunk*)
            (when body-cleanup (funcall body-cleanup))))))))

(defun list-body-chunk-to-octets (chunk)
  (typecase chunk
    (string (string-to-utf-8-bytes chunk))
    (null)
    (otherwise
     (warn "Invalid data in Clack response: ~S" chunk))))

(defun handle-normal-response (http socket clack-res &optional body-cleanup head-p)
  (flet ((send-error-response (status)
           ;; File preparation happens before headers are committed.  Close
           ;; the HTTP/1 connection after this one response so unread request
           ;; bytes cannot be parsed as another request.
           (wev:with-async-writing (socket :write-cb (lambda (socket)
                                                       (wev:graceful-close-socket socket)))
             (write-response-headers socket status
                                     '(:connection "close" :content-length 0))))
         (path-length-matches-p (headers size)
           (let ((declared (getf headers :content-length)))
             (or (null declared)
                 (handler-case
                     (= size (etypecase declared
                               (integer declared)
                               (string (parse-integer declared))))
                   (error () nil))))))
    (let ((no-body '#:no-body)
          (close (or (= (http-minor-version http) 0)
                     (string-equal (gethash "connection" (http-headers http)) "close"))))

    (destructuring-bind (status headers &optional (body no-body))
        clack-res
      (when (member status '(204 205 304))
        (setf body nil)
        (remf headers :transfer-encoding)
        (when (= status 204) (remf headers :content-length)))
      (when (eq body no-body)
        (setf (getf headers :transfer-encoding) "chunked")
        (setf (getf headers :content-length) nil)
        (wev:with-async-writing (socket)
          (write-response-headers socket status headers))
        (return-from handle-normal-response
          (make-streaming-writer socket body-cleanup head-p)))

      (etypecase body
        (null
         (wev:with-async-writing (socket :write-cb (and close
                                                        (lambda (socket)
                                                          (wev:graceful-close-socket socket))))
           (unless (or head-p (member status '(204 304)))
             (setf (getf headers :content-length) 0))
           (write-response-headers socket status headers (not close))))
        (pathname
         (let ((preflight-status (woo.http2.clack::pathname-response-status body)))
           (cond
             (preflight-status
             (send-error-response preflight-status))
             ((woo.ev.socket:socket-ssl-handle socket)
              (let ((headers-committed nil) (in nil))
                (unwind-protect
                     (handler-case
                         (progn
                           (setf in (open body :element-type '(unsigned-byte 8)))
                           (let ((size (file-length in)))
                             (unless (path-length-matches-p headers size)
                               (send-error-response 500)
                               (return-from handle-normal-response))
                             (unless (getf headers :content-length)
                               (setf (getf headers :content-length) size))
                             (unless (getf headers :content-type)
                               (setf (getf headers :content-type) (mimes:mime body)))
                             (wev:with-async-writing (socket :write-cb (and close #'wev:graceful-close-socket))
                               (setf headers-committed t)
                               (write-response-headers socket status headers (not close))
                               (if head-p
                                   (when body-cleanup (funcall body-cleanup))
                                   (progn
                                     (wev:start-static-stream socket in size)
                                     (setf in nil))))))
                       (file-error (e)
                         (if headers-committed (error e)
                             (let ((errno (ignore-errors (wsys:errno))))
                               (send-error-response
                                (woo.http2.clack::pathname-response-status body errno)))))
                       (error (e)
                         (if headers-committed (error e) (send-error-response 500))))
                  (when in (ignore-errors (close in :abort t))))))
             (t
              (let ((fd (wsys:open body)))
                (if (< fd 0)
                    (send-error-response
                     (let ((errno (wsys:errno)))
                       (woo.http2.clack::pathname-error-status errno)))
                    (let ((headers-committed nil))
                      (unwind-protect
                           (handler-case
                               (let ((size (progn
                                             #+lispworks (sys:file-size body)
                                             #+(or sbcl ccl) (fd-file-size fd)
                                             #-(or sbcl ccl lispworks) (file-size body))))
                                 (unless (path-length-matches-p headers size)
                                   (send-error-response 500)
                                   (return-from handle-normal-response))
                                 (unless (getf headers :content-length)
                                   (setf (getf headers :content-length) size))
                                 (unless (getf headers :content-type)
                                   (setf (getf headers :content-type) (mimes:mime body)))
                                 (wev:with-async-writing (socket :write-cb (and close
                                                                                (lambda (socket)
                                                                                  (wev:graceful-close-socket socket))))
                                   (setf headers-committed t)
                                   (write-response-headers socket status headers (not close))
                                   ;; SEND-STATIC-FILE takes ownership and closes
                                   ;; FD after the transfer completes.
                                   (if head-p
                                       (when body-cleanup (funcall body-cleanup))
                                       (progn
                                         (woo.ev.socket:send-static-file socket fd size)
                                         (setf fd nil)))))
                             (file-error (e)
                               (if headers-committed
                                   (error e)
                                   (send-error-response 500)))
                             (error (e)
                               (if headers-committed
                                   (error e)
                                   (send-error-response 500))))
                        (when fd (wsys:close fd))))))))))


        (list
         (wev:with-async-writing (socket :write-cb (and close
                                                        (lambda (socket)
                                                          (wev:graceful-close-socket socket))))
           (cond
             (head-p
              (unless (getf headers :content-length)
                (setf (getf headers :content-length)
                      (write-to-string
                       (loop for chunk in body
                             sum (if (stringp chunk)
                                     (utf-8-byte-length chunk)
                                     (length (list-body-chunk-to-octets chunk)))))))
              (response-headers-bytes socket status headers (not close))
              (write-socket-crlf socket))
             ((getf headers :content-length)
              (response-headers-bytes socket status headers (not close))
              (write-socket-crlf socket)
              (loop for chunk in body
                    for data = (list-body-chunk-to-octets chunk)
                    when data
                      do (wev:write-socket-data socket data)))
             (t
              (cond
                ((= (http-minor-version http) 1)
                 ;; Transfer-Encoding: chunked
                 (response-headers-bytes socket status headers (not close))
                 (wev:write-socket-data socket #.(string-to-utf-8-bytes "Transfer-Encoding: chunked"))
                 (write-socket-crlf socket)
                 (write-socket-crlf socket)
                 (loop for chunk in body
                       for data = (list-body-chunk-to-octets chunk)
                       when (and data (/= 0 (length data)))
                         do (write-socket-string socket (the simple-string (format nil "~X" (length data))))
                            (write-socket-crlf socket)
                            (wev:write-socket-data socket data)
                            (write-socket-crlf socket))
                 (wev:write-socket-byte socket #.(char-code #\0))
                 (write-socket-crlf socket)
                 (write-socket-crlf socket))
                (t
                 ;; calculate Content-Length
                 (response-headers-bytes socket status headers (not close))
                 (wev:write-socket-data socket #.(string-to-utf-8-bytes "Content-Length: "))
                 (write-socket-string
                  socket
                  (write-to-string (loop for chunk in body
                                         sum (if (stringp chunk)
                                                 (utf-8-byte-length chunk)
                                                 0))))
                 (write-socket-crlf socket)
                 (write-socket-crlf socket)
                 (loop for chunk in body
                       for data = (list-body-chunk-to-octets chunk)
                       when data
                         do (wev:write-socket-data socket data))))))))
        ((vector (unsigned-byte 8))
         (wev:with-async-writing (socket :write-cb (and close
                                                        (lambda (socket)
                                                          (wev:graceful-close-socket socket))))
           (response-headers-bytes socket status headers (not close))
           (unless (getf headers :content-length)
             (wev:write-socket-data socket #.(string-to-utf-8-bytes "Content-Length: "))
             (write-socket-string socket (write-to-string (length body)))
             (write-socket-crlf socket))
           (write-socket-crlf socket)
           (unless head-p (wev:write-socket-data socket body)))))))))

(defmethod clack.socket:read-callback ((socket woo.ev.socket:socket))
  (wev:socket-data socket))

(defmethod (setf clack.socket:read-callback) (callback (socket woo.ev.socket:socket))
  (setf (wev:socket-data socket) callback))

(defmethod clack.socket:write-sequence-to-socket ((socket woo.ev.socket:socket) data &key callback)
  (woo.ev.socket:check-socket-open socket)
  (wev:with-async-writing (socket :write-cb (and callback
                                                 (lambda (socket)
                                                   (declare (ignore socket))
                                                   (funcall callback))))
    (wev:write-socket-data socket data)))

(defmethod clack.socket:write-byte-to-socket ((socket woo.ev.socket:socket) byte &key callback)
  (woo.ev.socket:check-socket-open socket)
  (wev:with-async-writing (socket :write-cb (and callback
                                                 (lambda (socket)
                                                   (declare (ignore socket))
                                                   (funcall callback))))
    (wev:write-socket-byte socket byte)))

(defmethod clack.socket:write-sequence-to-socket-buffer ((socket woo.ev.socket:socket) data)
  (wev:write-socket-data socket data))

(defmethod clack.socket:write-byte-to-socket-buffer ((socket woo.ev.socket:socket) byte)
  (wev:write-socket-byte socket byte))

(defmethod clack.socket:flush-socket-buffer ((socket woo.ev.socket:socket) &key callback)
  (woo.ev.socket:check-socket-open socket)
  (wev:with-async-writing (socket :write-cb (and callback
                                                 (lambda (socket)
                                                   (declare (ignore socket))
                                                   (funcall callback))))
    nil))

(defmethod clack.socket:close-socket ((socket woo.ev.socket:socket))
  (when (woo.ev.socket:socket-open-p socket)
    (woo.ev.socket:close-socket socket)))

(defmethod clack.socket:socket-async-p ((socket woo.ev.socket:socket))
  t)
