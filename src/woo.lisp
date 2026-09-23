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
                :finalize-buffer)
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
                     ssl-key-password)
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
        (http2-handler nil))
    (labels ((ensure-http2-handler ()
               (unless http2-handler
                 (setf http2-handler (make-http2-app-handler *app*)))
               http2-handler)
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
                                          ssl-cert-file
                                          ssl-key-file
                                          ssl-key-password))
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
                     (signal-watchers (make-signal-watchers)))
                 (wev:with-sockaddr
                   (unwind-protect
                        (wev:with-event-loop (:cleanup-fn
                                              (lambda ()
                                                (stop-signal-watchers *evloop* signal-watchers)))
                          (start-signal-watchers *evloop* signal-watchers)
                          (setq *listener*
                                (wev:tcp-server (or listen
                                                    (cons address port))
                                                #'read-cb
                                                :connect-cb
                                                (lambda (socket)
                                                  (woo.worker:add-job-to-cluster *cluster* socket))
                                                :backlog backlog
                                                :fd fd
                                                :sockopt wsock:+SO-REUSEADDR+)))
                     (wev:close-tcp-server *listener*)
                     (woo.worker:stop-cluster *cluster*)))))
             (start-singlethread-server ()
               (let ((signal-watchers (make-signal-watchers)))
                 (wev:with-sockaddr
                   (unwind-protect
                        (wev:with-event-loop (:cleanup-fn
                                              (lambda ()
                                                (stop-signal-watchers *evloop* signal-watchers)))
                          (start-signal-watchers *evloop* signal-watchers)
                          (setq *listener*
                                (wev:tcp-server (or listen
                                                    (cons address port))
                                                #'read-cb
                                                :connect-cb #'start-socket
                                                :backlog backlog
                                                :fd fd
                                                :sockopt wsock:+SO-REUSEADDR+)))
                     (wev:close-tcp-server *listener*))))))
      (when ssl
        #+woo-no-ssl
        (warn "SSL certificate is specified but Woo's SSL feature is off. Ignored.")
        #-woo-no-ssl
        (progn
          (cl+ssl::ensure-initialized)
          (when ssl-key-file
            (setf ssl-key-file
                  (uiop:native-namestring
                   (or (probe-file ssl-key-file)
                       (error "SSL private key file '~A' does not exist." ssl-key-file)))))
          (when ssl-cert-file
            (setf ssl-cert-file
                  (uiop:native-namestring
                   (or (probe-file ssl-cert-file)
                       (error "SSL certificate '~A' does not exist." ssl-cert-file)))))))
      (if worker-num
          (start-multithread-server)
          (start-singlethread-server)))))

(defun read-cb (socket data &key (start 0) (end (length data)))
  (let ((parser (wev:socket-data socket)))
    (handler-case (funcall parser data :start start :end end)
      (fast-http:parsing-error (e)
        (vom:error "HTTP parse error: ~A" e)
        (let ((body #.(map '(simple-array (unsigned-byte 8) (*))
                           #'char-code
                           "400 Bad Request")))
          (wev:with-async-writing (socket :write-cb #'wev:close-socket)
            (write-response-headers socket 400
                                    (list :connection "close"
                                          :content-length (length body)))
            (wev:write-socket-data socket body)))))))

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

(defun setup-parser (socket)
  ;; A request with an Upgrade header may be followed, in the same read, by
  ;; octets of the new protocol: a WebSocket client need not wait for the
  ;; 101. fast-http stops at the end of such a request but does not say
  ;; where that is, so the reader feeds it one piece at a time, each ending
  ;; at a CR LF CR LF (every header block ends there) or at the end of the
  ;; request's Content-Length body. Once the application has upgraded the
  ;; socket, or is still deciding (a delayed response), the rest of the
  ;; read and every later one go to FEED-WEBSOCKET-DATA, never to fast-http.
  (let ((http (make-http-request))
        (body-buffer (make-smart-buffer))
        ;; The request being parsed carries an Upgrade header.
        (upgrade-request nil)
        ;; Octets of that request's body not yet fed to fast-http.
        (body-remaining 0)
        ;; An upgrade request's delayed response has not been given yet.
        (response-pending nil)
        ;; Octets are being held for WebSocket while RESPONSE-PENDING.
        (holding nil)
        ;; Length of the CR LF CR LF prefix that ends the octets scanned.
        (crlf-match 0)
        parser
        reader)
    (declare (type fixnum body-remaining crlf-match))
    (labels ((next-split (data start end)
               (declare (type (simple-array (unsigned-byte 8) (*)) data)
                        (type fixnum start end))
               (if (plusp body-remaining)
                   (let ((split (min end (+ start body-remaining))))
                     (decf body-remaining (- split start))
                     (setq crlf-match 0)
                     split)
                   (loop for i of-type fixnum from start below end
                         for b = (aref data i)
                         do (setq crlf-match
                                  (cond ((= b (if (evenp crlf-match) 13 10)) (1+ crlf-match))
                                        ((= b 13) 1)
                                        (t 0)))
                            (when (= crlf-match 4)
                              ;; The trailing CR LF may start the next match.
                              (setq crlf-match 2)
                              (return (1+ i)))
                         finally (return end))))
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
                         :header-callback
                         (lambda (headers)
                           (declare (ignore headers))
                           (when (http-upgrade-p http)
                             (setq upgrade-request t)
                             ;; fast-http would take a Content-Length body for
                             ;; the new protocol's first octets. Let it read the
                             ;; body; NEXT-SPLIT stops the piece where it ends.
                             ;; A chunked body cannot be delimited that way.
                             (unless (http-chunked-p http)
                               (setf (http-upgrade-p http) nil)
                               (let ((n (http-content-length http)))
                                 (when (and (integerp n) (plusp n))
                                   (setq body-remaining n))))))
                         :body-callback
                         (lambda (data start end)
                           (declare (type (simple-array (unsigned-byte 8) (*)) data))
                           (if (smart-buffer::buffer-on-memory-p body-buffer)
                               (write-to-buffer body-buffer (subseq data start end) 0 (- end start))
                               (write-to-buffer body-buffer data start end)))
                         :finish-callback
                         (lambda ()
                           (let ((upgrading upgrade-request))
                             ;; fast-http never clears the flag itself.
                             (setq upgrade-request nil)
                             (setf (http-upgrade-p http) nil)
                             (flet ((main (env)
                                      (handle-response http socket
                                                       (watch-response
                                                        upgrading
                                                        (if *debug*
                                                            (funcall *app* env)
                                                            (if-let (res (handler-case (funcall *app* env)
                                                                           (error (error)
                                                                             (vom:error (princ-to-string error))
                                                                             nil)))
                                                                    res
                                                                    '(500 nil nil)))))))
                               (block result
                                 (let ((raw-body (finalize-buffer body-buffer)))
                                   (setq body-buffer (make-smart-buffer))
                                   (handler-bind
                                       ((error ;; handle errors inside woo
                                          (lambda (e)
                                            (unless *debug*
                                              (vom:crit (princ-to-string e))
                                              (return-from result (handle-response http socket '(500 nil nil)))))))
                                     (let ((env (nconc (list :raw-body raw-body)
                                                       (handle-request http socket))))
                                       (main env))))))))))
      (setq reader
            (lambda (data &key (start 0) (end (length data)))
              (declare (type (simple-array (unsigned-byte 8) (*)) data)
                       (type fixnum start end))
              (loop
                (when (>= start end)
                  (return))
                ;; No HTTP parsing once the socket belongs to WebSocket.
                (when (or (socket-upgraded-p socket) response-pending)
                  (return (hold-for-websocket data start end)))
                (let ((split (next-split data start end)))
                  (funcall parser data :start start :end split)
                  (setq start split)))))
      (setf (wev:socket-data socket) reader))))

(defun stop (server)
  (wev:close-tcp-server server))


;;
;; Handling requests

(defun parse-host-header (host)
  (declare (type simple-string host)
           (optimize (speed 3) (safety 0)))
  (let ((pos (position #\: host :from-end t)))
    (unless pos
      (return-from parse-host-header
        (values host nil)))

    (locally (declare (type fixnum pos))
      (let ((port (loop with port of-type fixnum = 0
                        for i from (1+ pos) to (1- (length host))
                        for char = (aref host i)
                        do (if (digit-char-p char)
                               (setq port (+ (* 10 port)
                                             (- (char-code char) (char-code #\0))))
                               (return nil))
                        finally
                           (return port))))
        (if port
            (values (subseq host 0 pos)
                    port)
            (values host nil))))))

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

(defun handle-response (http socket clack-res)
  ;; After a WebSocket upgrade the socket carries frames, not HTTP. Whatever
  ;; the app returned (NIL, which becomes a 500, or a framework's finalized
  ;; 200), writing it would corrupt the stream.
  (when (socket-upgraded-p socket)
    (return-from handle-response nil))
  (handler-case
      (etypecase clack-res
        (list (handle-normal-response http socket clack-res))
        (function (funcall clack-res (lambda (clack-res)
                                       (unless (socket-upgraded-p socket)
                                         (handler-case
                                             (handle-normal-response http socket clack-res)
                                           (wev:socket-closed ())))))))
    (wev:tcp-error (e)
      (vom:error (princ-to-string e)))))

#+sbcl
(defvar *stat* (make-instance 'sb-posix:stat))
#+sbcl
(defun fd-file-size (fd)
  (sb-posix:fstat fd *stat*)
  (sb-posix:stat-size *stat*))
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

(defun make-streaming-writer (socket)
  (lambda (body &key (start 0 has-start) (end nil has-end) (close nil))
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
            (finish-response socket *empty-chunk*)))
        (when close
          (wev:with-async-writing (socket)
            (finish-response socket *empty-chunk*))))))

(defun list-body-chunk-to-octets (chunk)
  (typecase chunk
    (string (string-to-utf-8-bytes chunk))
    (null)
    (otherwise
     (warn "Invalid data in Clack response: ~S" chunk))))

(defun handle-normal-response (http socket clack-res)
  (let ((no-body '#:no-body)
        (close (or (= (http-minor-version http) 0)
                   (string-equal (gethash "connection" (http-headers http)) "close"))))
    (destructuring-bind (status headers &optional (body no-body))
        clack-res
      (when (eq body no-body)
        (setf (getf headers :transfer-encoding) "chunked")
        (setf (getf headers :content-length) nil)
        (wev:with-async-writing (socket)
          (write-response-headers socket status headers))
        (return-from handle-normal-response
          (make-streaming-writer socket)))

      (etypecase body
        (null
         (wev:with-async-writing (socket :write-cb (and close
                                                        (lambda (socket)
                                                          (wev:close-socket socket))))
           (unless (= status 304)
             (setf (getf headers :content-length) 0))
           (write-response-headers socket status headers (not close))))
        (pathname
         (cond
           ((woo.ev.socket:socket-ssl-handle socket)
            (with-open-file (in body :element-type '(unsigned-byte 8))
              (let ((size (file-length in)))
                (unless (getf headers :content-length)
                  (setf (getf headers :content-length) size))
                (unless (getf headers :content-type)
                  (setf (getf headers :content-type) (mimes:mime body)))
                (wev:with-async-writing (socket :write-cb (and close
                                                               (lambda (socket)
                                                                 (wev:close-socket socket))))
                  (write-response-headers socket status headers (not close))
                  ;; Future task: Use OpenSSL's SSL_sendfile which uses Kernel TLS.
                  (wev:write-socket-stream socket in)))))
           (t
            (let* ((fd (wsys:open body))
                   (size #+lispworks (sys:file-size body)
                         #+(or sbcl ccl) (fd-file-size fd)
                         #-(or sbcl ccl lispworks) (file-size body)))
              (unless (getf headers :content-length)
                (setf (getf headers :content-length) size))
              (unless (getf headers :content-type)
                (setf (getf headers :content-type) (mimes:mime body)))
              (wev:with-async-writing (socket :write-cb (and close
                                                             (lambda (socket)
                                                               (wev:close-socket socket))))
                (write-response-headers socket status headers (not close))
                (woo.ev.socket:send-static-file socket fd size))))))
        (list
         (wev:with-async-writing (socket :write-cb (and close
                                                        (lambda (socket)
                                                          (wev:close-socket socket))))
           (cond
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
                                                          (wev:close-socket socket))))
           (response-headers-bytes socket status headers (not close))
           (unless (getf headers :content-length)
             (wev:write-socket-data socket #.(string-to-utf-8-bytes "Content-Length: "))
             (write-socket-string socket (write-to-string (length body)))
             (write-socket-crlf socket))
           (write-socket-crlf socket)
           (wev:write-socket-data socket body)))))))

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
