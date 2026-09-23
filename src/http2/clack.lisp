(in-package :cl-user)
(defpackage woo.http2.clack
  (:use :cl
        :woo.http2.constants
        :woo.http2.frames
        :woo.http2.hpack
        :woo.http2.stream
        :woo.http2.connection)
  (:import-from :woo.ev.socket
                :socket
                :socket-remote-addr
                :socket-remote-port
                :socket-open-p)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes)
  (:export :make-http2-app-handler
           :request-method-keyword
           :*pathname-chunk-size*
           :*pathname-body-open-hook*
           :attach-http2-app
           :build-clack-env
           :send-http2-response
           :validate-request-headers
           :combine-header-fields
           :connection-send-max-frame-size
           ;; Defined in woo.http2.connection; re-exported for callers.
           :*http2-frame-sink*))
(in-package :woo.http2.clack)

(defun emit-frame (conn frame)
  "Send FRAME. connection-send-frame reports it to *http2-frame-sink*."
  (connection-send-frame conn frame)
  frame)

(defun connection-send-max-frame-size (conn)
  "Payload limit for outbound frames: min of local and remote SETTINGS_MAX_FRAME_SIZE."
  (max 1 (min (http2-connection-local-max-frame-size conn)
              (http2-connection-remote-max-frame-size conn))))

(defun pseudo-header-p (name)
  "Check if header name is a pseudo-header (starts with :)."
  (and (stringp name)
       (> (length name) 0)
       (char= (char name 0) #\:)))

(defun request-pseudo-name-p (name)
  (member name '(":method" ":scheme" ":path" ":authority") :test #'string=))

(defun valid-request-path-p (path)
  ":path is \"*\" or an absolute path (RFC 9113 §8.3.1)."
  (or (string= path "*")
      (and (plusp (length path))
           (char= (char path 0) #\/))))

(defun validate-request-headers (headers)
  "Return T if HEADERS is a valid HTTP/2 request header list, else NIL.
   Enforces pseudo-header order, required set, no response :status, no
   connection-specific fields, token names, and :path / host agreement
   (RFC 9113 §8.2–8.3)."
  (let ((seen-regular nil)
        (authority nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (header headers)
      (let ((name (car header))
            (value (cdr header)))
        (unless (and (stringp name) (stringp value) (> (length name) 0))
          (return-from validate-request-headers nil))
        (unless (field-value-ok-p value)
          (return-from validate-request-headers nil))
        (when (find-if #'upper-case-p name)
          (return-from validate-request-headers nil))
        (cond
          ((pseudo-header-p name)
           (when seen-regular
             (return-from validate-request-headers nil))
           (unless (request-pseudo-name-p name)
             (return-from validate-request-headers nil))
           (when (gethash name seen)
             (return-from validate-request-headers nil))
           (setf (gethash name seen) t)
           (when (zerop (length value))
             (return-from validate-request-headers nil))
           (when (string= name ":path")
             (unless (valid-request-path-p value)
               (return-from validate-request-headers nil)))
           (when (string= name ":authority")
             (setf authority value)))
          (t
           (unless (http-token-name-p name)
             (return-from validate-request-headers nil))
           (setf seen-regular t)
           (when (and authority
                      (string= name "host")
                      (not (string-equal value authority)))
             (return-from validate-request-headers nil))
           (when (connection-specific-header-p name value)
             (return-from validate-request-headers nil))))))
    (and (gethash ":method" seen)
         (gethash ":scheme" seen)
         (gethash ":path" seen))))

(defun combine-header-fields (headers)
  "Combine duplicate regular header fields. Cookie uses \"; \", others \", \"."
  (let ((order nil)
        (table (make-hash-table :test 'equal)))
    (dolist (header headers)
      (let ((name (car header))
            (value (cdr header)))
        (if (pseudo-header-p name)
            (push header order)
            (let ((prev (gethash name table)))
              (if prev
                  (setf (gethash name table)
                        (if (string= name "cookie")
                            (concatenate 'string prev "; " value)
                            (concatenate 'string prev ", " value)))
                  (progn
                    (setf (gethash name table) value)
                    (push name order)))))))
    (nreverse
     (mapcar (lambda (item)
               (if (consp item)
                   item
                   (cons item (gethash item table))))
             order))))

;; The methods fast-http accepts for HTTP/1, so both protocols give an
;; application the same set. Methods are case-sensitive (RFC 9110 §9.1).
;; A client-chosen string is never interned: that would grow the heap
;; without bound.
(defparameter *request-methods*
  (let ((table (make-hash-table :test 'equal)))
    (dolist (method '(:CHECKOUT :CONNECT :COPY :DELETE :GET :HEAD :LOCK
                      :M-SEARCH :MERGE :MKACTIVITY :MKCALENDAR :MKCOL :MOVE
                      :NOTIFY :OPTIONS :PATCH :POST :PROPFIND :PROPPATCH
                      :PURGE :PUT :REPORT :SEARCH :SUBSCRIBE :TRACE :UNLOCK
                      :UNSUBSCRIBE)
                    table)
      (setf (gethash (symbol-name method) table) method))))

(defun request-method-keyword (method)
  "The keyword for a known request method, else NIL."
  (values (gethash method *request-methods*)))

(defun build-clack-env (socket stream headers)
  "Build Clack environment from HTTP/2 stream headers.
   Returns NIL if the header list is not a valid HTTP/2 request."
  (unless (validate-request-headers headers)
    (return-from build-clack-env nil))
  (let ((headers (combine-header-fields headers))
        (env (list :clack.streaming t
                   :clack.nonblocking t
                   :clack.io socket
                   :http2.stream stream
                   :http2.connection nil
                   :server-protocol :HTTP/2
                   :script-name ""
                   :remote-addr (and socket (socket-remote-addr socket))
                   :remote-port (and socket (socket-remote-port socket))))
        (http-headers (make-hash-table :test 'equal)))

    (dolist (header headers)
      (let ((name (car header))
            (value (cdr header)))
        (cond
          ((string= name ":method")
           ;; NIL for a method we do not know; the request gets 501.
           (setf (getf env :request-method) (request-method-keyword value)))
          ((string= name ":path")
           (let* ((path value)
                  (query-pos (position #\? path)))
             (if query-pos
                 (setf (getf env :path-info) (quri:url-decode (subseq path 0 query-pos) :lenient t)
                       (getf env :query-string) (subseq path (1+ query-pos))
                       (getf env :request-uri) path)
                 (setf (getf env :path-info) (quri:url-decode path :lenient t)
                       (getf env :request-uri) path))))
          ((string= name ":scheme")
           (setf (getf env :url-scheme) value))
          ((string= name ":authority")
           (setf env (apply-authority env value)))
          ((string= name "content-type")
           (setf (getf env :content-type) value)
           (setf (gethash name http-headers) value))
          ((string= name "content-length")
           (setf (getf env :content-length) (parse-integer value :junk-allowed t))
           (setf (gethash name http-headers) value))
          ((string= name "host")
           (setf (gethash name http-headers) value)
           (unless (getf env :server-name)
             (setf env (apply-authority env value))))
          ((not (pseudo-header-p name))
           (setf (gethash name http-headers) value)))))

    ;; Port 0 is real; only an absent port takes the scheme default.
    (unless (integerp (getf env :server-port))
      (setf (getf env :server-port)
            (if (string= (getf env :url-scheme) "https") 443 80)))
    (unless (getf env :path-info)
      (setf (getf env :path-info) "/"))
    (unless (getf env :request-uri)
      (setf (getf env :request-uri) (getf env :path-info)))

    (setf (getf env :headers) http-headers)
    env))

(defun parse-decimal-port (string start)
  "Port digits at START, or NIL. Junk after a number is ignored, matching Host."
  (when (and (< start (length string))
             (digit-char-p (char string start)))
    (parse-integer string :start start :junk-allowed t)))

(defun split-authority (authority)
  "Split :authority or Host into host and port.
   Bracketed IPv6 keeps its brackets. The port is only the number after
   the closing bracket, so \"[::1]\" is not host \"[:\" port 1.
   Returns (values host port). PORT is NIL when absent."
  (let ((len (length authority)))
    (cond
      ((and (plusp len) (char= (char authority 0) #\[))
       (let ((end (position #\] authority)))
         (cond
           ((null end)
            (values authority nil))
           ((and (< (1+ end) len)
                 (char= (char authority (1+ end)) #\:))
            (values (subseq authority 0 (1+ end))
                    (parse-decimal-port authority (+ end 2))))
           (t
            (values (subseq authority 0 (1+ end)) nil)))))
      (t
       (let ((colon (position #\: authority :from-end t)))
         (if (and colon (plusp colon))
             (let ((port (parse-decimal-port authority (1+ colon))))
               (if (integerp port)
                   (values (subseq authority 0 colon) port)
                   (values authority nil)))
             (values authority nil)))))))

(defun apply-authority (env value)
  "Return ENV with :server-name and :server-port from VALUE.
   The caller must use the result: SETF GETF on a new key conses onto the
   front of the plist, which a callee's parameter cannot hand back."
  (multiple-value-bind (host port) (split-authority value)
    (setf (getf env :server-name) host)
    ;; Port 0 is a real port. Only NIL means "absent".
    (when (integerp port)
      (setf (getf env :server-port) port))
    env))

;;; Running response writes on the connection's event loop.
;;;
;;; A delayed response may call the responder, and the streaming writer it
;;; returns, from any thread. Connection state, the HPACK encoder and the
;;; socket buffer belong to the event loop, so writes from another thread
;;; are queued and the loop is woken with an ev_async watcher, one per loop.
;;; When the loop stops, its dispatcher is marked stopped under its lock,
;;; so no thread wakes a loop that is being freed.

(defstruct (loop-dispatcher (:conc-name dispatcher-))
  (id 0 :type fixnum)
  evloop
  watcher
  (lock (bt2:make-lock :name "woo HTTP/2 dispatcher"))
  ;; Thunks waiting to run on the loop, newest first.
  (thunks nil :type list)
  ;; Set, under LOCK, before the loop and the watcher are freed.
  (stopped nil))

(defvar *dispatchers* (make-hash-table)
  "Dispatcher id -> dispatcher. The id is kept in the loop's ev_userdata.")
(defvar *dispatchers-lock* (bt2:make-lock :name "woo HTTP/2 dispatchers"))
(defvar *dispatcher-counter* 0)

;; lev does not bind ev_unref. The async watcher must not keep ev_run alive
;; once the listener stops.
(cffi:defcfun ("ev_unref" %ev-unref) :void (evloop :pointer))

(defun find-dispatcher (evloop)
  (let ((userdata (lev:ev-userdata evloop)))
    (unless (cffi:null-pointer-p userdata)
      (bt2:with-lock-held (*dispatchers-lock*)
        (gethash (cffi:pointer-address userdata) *dispatchers*)))))

(defun drain-dispatcher (dispatcher)
  "Run queued thunks in the order they were queued."
  (loop
    (let ((thunks (bt2:with-lock-held ((dispatcher-lock dispatcher))
                    (prog1 (nreverse (dispatcher-thunks dispatcher))
                      (setf (dispatcher-thunks dispatcher) nil)))))
      (unless thunks (return))
      (dolist (thunk thunks)
        (handler-case (funcall thunk)
          (error (e)
            (vom:error "Error in HTTP/2 response write: ~A" e)))))))

(cffi:defcallback http2-dispatch-cb :void ((evloop :pointer) (watcher :pointer) (events :int))
  (declare (ignore watcher events))
  (let ((dispatcher (find-dispatcher evloop)))
    (when dispatcher
      (drain-dispatcher dispatcher))))

(defun dispatcher-enqueue (dispatcher thunk)
  "Queue THUNK to run on DISPATCHER's loop and wake the loop. Returns T, or
   NIL, dropping THUNK, once the loop has stopped. The check and the wakeup
   hold the lock that stop-dispatcher takes before the loop is freed."
  (bt2:with-lock-held ((dispatcher-lock dispatcher))
    (unless (dispatcher-stopped dispatcher)
      (push thunk (dispatcher-thunks dispatcher))
      (lev:ev-async-send (dispatcher-evloop dispatcher)
                         (dispatcher-watcher dispatcher))
      t)))

(defun stop-dispatcher (dispatcher)
  "Called on the loop's thread after ev_run returns, before the loop is
   freed. Later enqueues are refused. Queued thunks are dropped: they would
   write to sockets the loop is closing, and they hold connections and body
   octets. The watcher is stopped and freed, and the dispatcher forgotten."
  (let ((watcher nil)
        (evloop nil))
    (bt2:with-lock-held ((dispatcher-lock dispatcher))
      (setf (dispatcher-stopped dispatcher) t
            (dispatcher-thunks dispatcher) nil
            watcher (dispatcher-watcher dispatcher)
            evloop (dispatcher-evloop dispatcher)
            (dispatcher-watcher dispatcher) nil
            (dispatcher-evloop dispatcher) nil))
    (bt2:with-lock-held (*dispatchers-lock*)
      (remhash (dispatcher-id dispatcher) *dispatchers*))
    (when evloop
      (lev:ev-set-userdata evloop (cffi:null-pointer))
      (when watcher
        ;; Undo the ev_unref done at start, as libev requires before a stop.
        (lev:ev-ref evloop)
        (lev:ev-async-stop evloop watcher)
        (cffi:foreign-free watcher)))
    nil))

(defun current-loop-dispatcher ()
  "The dispatcher of the event loop running in this thread, made on first
   use. NIL outside an event loop. A fresh loop has NULL ev_userdata, so a
   loop allocated at a freed loop's address does not find the old one. The
   dispatcher is stopped when the loop exits (woo.ev.event-loop's
   *evloop-exit-hooks*)."
  (let ((evloop woo.ev.event-loop:*evloop*))
    (when (and evloop (not (cffi:null-pointer-p evloop)))
      (or (find-dispatcher evloop)
          (let* ((id (bt2:with-lock-held (*dispatchers-lock*)
                       (incf *dispatcher-counter*)))
                 (watcher (cffi:foreign-alloc '(:struct lev:ev-async)))
                 (dispatcher (make-loop-dispatcher :id id :evloop evloop
                                                   :watcher watcher)))
            (lev:ev-async-init watcher 'http2-dispatch-cb)
            (lev:ev-async-start evloop watcher)
            (%ev-unref evloop)
            (bt2:with-lock-held (*dispatchers-lock*)
              (setf (gethash id *dispatchers*) dispatcher))
            (lev:ev-set-userdata evloop (cffi:make-pointer id))
            (push (lambda () (stop-dispatcher dispatcher))
                  woo.ev.event-loop:*evloop-exit-hooks*)
            dispatcher)))))

(defun http2-socket-accepts-p (socket)
  (or (null socket) (socket-open-p socket)))

(defun dispatcher-runner (dispatcher socket)
  "Call on DISPATCHER's loop thread. A function that runs a thunk on that
   loop and returns T, or NIL when the thunk was dropped because SOCKET is
   closed or the loop has stopped. On the loop thread the thunk runs at
   once, after any thunks other threads queued before it, so writes to a
   response keep the order they were made in."
  (let ((owner (bt2:current-thread)))
    (lambda (thunk)
      (cond
        ((eq (bt2:current-thread) owner)
         (drain-dispatcher dispatcher)
         (funcall thunk)
         t)
        ;; The loop closes its sockets before it is freed, so a closed
        ;; socket means there may be no loop left to wake.
        ((http2-socket-accepts-p socket)
         (dispatcher-enqueue dispatcher thunk))))))

(defun make-loop-runner (socket)
  "Call on the connection's event-loop thread. Returns a function that runs
   a thunk on that loop (see dispatcher-runner). Without an event loop (unit
   tests) the thunk runs at once."
  (let ((dispatcher (and socket (current-loop-dispatcher))))
    (if dispatcher
        (dispatcher-runner dispatcher socket)
        (lambda (thunk)
          (funcall thunk)
          t))))

;;; Sending responses

(defparameter *pathname-chunk-size* 16384
  "Octets read from a pathname body at a time. The file is read only as the
   send window allows, and it is not held open while waiting for credit.")

(defvar *pathname-body-open-hook* nil
  "When non-nil, called with each file stream opened to read a pathname body.")

(defun connection-closing-p (conn)
  (woo.http2.connection::http2-connection-closing conn))

(defun stream-sendable-p (conn stream)
  "True while frames may be sent on STREAM: the connection is not closing
   and the stream has not been closed or ended by us."
  (and (not (connection-closing-p conn))
       (not (stream-closed-p stream))
       (/= (http2-stream-state stream) +state-half-closed-local+)))

(defun send-header-block (conn stream header-block &key end-stream)
  "Send HEADER-BLOCK as HEADERS plus CONTINUATION frames at max frame size.
   Nothing is sent on a stream that is closed or already ended. Returns T
   when sent."
  (unless (stream-sendable-p conn stream)
    (return-from send-header-block nil))
  (let* ((stream-id (http2-stream-id stream))
         (max (connection-send-max-frame-size conn))
         (len (length header-block))
         (offset 0)
         (first t))
    (loop
      (let* ((remaining (- len offset))
             (chunk (min max remaining))
             (last (>= (+ offset chunk) len))
             (fragment (if (and (zerop offset) last)
                           header-block
                           (subseq header-block offset (+ offset chunk)))))
        (emit-frame conn
                    (if first
                        (make-headers-frame stream-id fragment
                                            :end-headers last
                                            :end-stream end-stream)
                        (make-continuation-frame stream-id fragment
                                                 :end-headers last)))
        (setf first nil)
        (incf offset chunk)
        (when last (return))))
    t))

(defun send-window-available (conn stream)
  (max 0 (min (http2-connection-remote-window-size conn)
              (http2-stream-window-size stream))))

(defun consume-send-window (conn stream n)
  (decf (http2-connection-remote-window-size conn) n)
  (decf (http2-stream-window-size stream) n))

(defstruct (pending-response (:conc-name pending-))
  "A response whose HEADERS are sent and whose body is not finished. It is
   the stream's entry in the connection send queue until END_STREAM."
  ;; Octet vectors still to send, in order, and the last cons for appends.
  (chunks nil :type list)
  (tail nil :type list)
  ;; Octets of the first chunk already sent.
  (offset 0 :type fixnum)
  ;; A pathname body, sent from PATH-OFFSET up to PATH-END.
  (path nil)
  (path-offset 0 :type integer)
  (path-end 0 :type integer)
  ;; END_STREAM follows the octets above. NIL while a writer may add more.
  (end-stream nil))

(defun pending-path-done-p (pending)
  (or (null (pending-path pending))
      (>= (pending-path-offset pending) (pending-path-end pending))))

(defun pending-empty-p (pending)
  (and (null (pending-chunks pending))
       (pending-path-done-p pending)))

(defun pending-append (pending octets)
  (when (plusp (length octets))
    (let ((cell (list octets)))
      (if (pending-chunks pending)
          (setf (cdr (pending-tail pending)) cell)
          (setf (pending-chunks pending) cell))
      (setf (pending-tail pending) cell))))

(defun empty-octets ()
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun send-pending-chunks (conn stream pending)
  "Send queued chunks as the window allows. Returns :finished when the last
   frame carried END_STREAM, :blocked when out of window, else NIL."
  (let ((id (http2-stream-id stream))
        (max (connection-send-max-frame-size conn)))
    (loop while (pending-chunks pending)
          do (let* ((chunk (first (pending-chunks pending)))
                    (len (length chunk)))
               (loop while (< (pending-offset pending) len)
                     do (let* ((start (pending-offset pending))
                               (n (min max (send-window-available conn stream)
                                       (- len start))))
                          (when (<= n 0)
                            (return-from send-pending-chunks :blocked))
                          (let* ((end (+ start n))
                                 (last (and (= end len)
                                            (null (rest (pending-chunks pending)))
                                            (pending-path-done-p pending)
                                            (pending-end-stream pending))))
                            (emit-frame conn
                                        (make-data-frame id
                                                         (if (and (zerop start) (= end len))
                                                             chunk
                                                             (subseq chunk start end))
                                                         :end-stream last))
                            (consume-send-window conn stream n)
                            (setf (pending-offset pending) end)
                            (when last
                              (return-from send-pending-chunks :finished)))))
               (pop (pending-chunks pending))
               (setf (pending-offset pending) 0)
               (unless (pending-chunks pending)
                 (setf (pending-tail pending) nil))))
    nil))

(defun send-pending-path (conn stream pending)
  "Send the pathname body as the window allows, reading at most
   *pathname-chunk-size* octets at a time. The file is open only during this
   call. Returns :finished, :blocked, :failed (the file got shorter), or NIL."
  (when (pending-path-done-p pending)
    (return-from send-pending-path nil))
  (when (<= (send-window-available conn stream) 0)
    (return-from send-pending-path :blocked))
  (let ((id (http2-stream-id stream))
        (max (connection-send-max-frame-size conn)))
    (with-open-file (in (pending-path pending) :element-type '(unsigned-byte 8))
      (when *pathname-body-open-hook*
        (funcall *pathname-body-open-hook* in))
      (file-position in (pending-path-offset pending))
      (loop until (pending-path-done-p pending)
            do (let ((n (min *pathname-chunk-size* max
                             (send-window-available conn stream)
                             (- (pending-path-end pending)
                                (pending-path-offset pending)))))
                 (when (<= n 0)
                   (return-from send-pending-path :blocked))
                 (let* ((buf (make-array n :element-type '(unsigned-byte 8)))
                        (got (read-sequence buf in)))
                   (when (< got n)
                     (return-from send-pending-path :failed))
                   (incf (pending-path-offset pending) n)
                   (let ((last (and (pending-path-done-p pending)
                                    (pending-end-stream pending))))
                     (emit-frame conn (make-data-frame id buf :end-stream last))
                     (consume-send-window conn stream n)
                     (when last
                       (return-from send-pending-path :finished)))))))
    nil))

(defun pump-response (conn stream pending)
  "Send what the window allows. Returns :finished once END_STREAM is sent."
  (let ((result (send-pending-chunks conn stream pending)))
    (when result
      (return-from pump-response result)))
  (let ((result (send-pending-path conn stream pending)))
    (when result
      (return-from pump-response result)))
  ;; Everything queued is sent. END_STREAM rides an empty DATA frame when
  ;; the writer closed after its last octets went out.
  (when (pending-end-stream pending)
    (emit-frame conn (make-data-frame (http2-stream-id stream) (empty-octets)
                                      :end-stream t))
    :finished))

(defun note-response-finished (conn stream)
  "Our END_STREAM has been sent. A request that already ended moves to closed
   and is dropped from the open table so it no longer counts as concurrent."
  (cond
    ((= (http2-stream-state stream) +state-half-closed-remote+)
     (stream-transition stream :send-end-stream))
    ((= (http2-stream-state stream) +state-open+)
     (stream-transition stream :send-end-stream)))
  (when (stream-closed-p stream)
    (connection-drop-closed-stream conn stream)))

(defun advance-response (conn stream)
  "Send what the window allows of STREAM's pending response. Returns T once
   the response is complete. A reset or closed stream just loses its entry."
  (let* ((queue (http2-connection-send-queue conn))
         (id (http2-stream-id stream))
         (pending (gethash id queue)))
    (cond
      ((null pending) nil)
      ((not (stream-sendable-p conn stream))
       (remhash id queue)
       nil)
      (t
       (ecase (pump-response conn stream pending)
         (:finished
          (remhash id queue)
          (note-response-finished conn stream)
          t)
         (:failed
          (vom:error "HTTP/2 pathname body ~A got shorter while being sent"
                     (pending-path pending))
          (remhash id queue)
          (connection-stream-error conn stream +internal-error+)
          nil)
         ((:blocked nil) nil))))))

(defun ensure-send-flush-hook (conn)
  (unless (http2-connection-flush-sends conn)
    (setf (http2-connection-flush-sends conn) #'flush-pending-response-data)))

(defun flush-pending-response-data (conn &optional only-stream)
  "Write queued DATA now that a send window has grown. END_STREAM rides the last frame."
  (if only-stream
      (advance-response conn only-stream)
      (dolist (id (let ((ids nil))
                    (maphash (lambda (id entry)
                               (declare (ignore entry))
                               (push id ids))
                             (http2-connection-send-queue conn))
                    (sort ids #'<)))
        (let ((stream (gethash id (http2-connection-streams conn))))
          (if stream
              (advance-response conn stream)
              (remhash id (http2-connection-send-queue conn)))))))

(defun response-startable-p (conn stream)
  "True when no response has begun on STREAM and frames may still be sent."
  (and (stream-sendable-p conn stream)
       (null (gethash (http2-stream-id stream)
                      (http2-connection-send-queue conn)))))

(defun octets-of (data &key (start 0) end)
  "A fresh octet vector for a body chunk: strings as UTF-8, octets copied,
   so the caller may reuse its buffer."
  (etypecase data
    (null (empty-octets))
    (string (string-to-utf-8-bytes (if (or (/= start 0) end)
                                       (subseq data start end)
                                       data)))
    ((vector (unsigned-byte 8))
     (let ((out (make-array (- (or end (length data)) start)
                            :element-type '(unsigned-byte 8))))
       (replace out data :start2 start :end2 end)
       out))))

(defun body-chunks (body)
  "The non-empty octet vectors of a string, octet vector, or list body."
  (flet ((chunk (part)
           (etypecase part
             (null nil)
             (string (string-to-utf-8-bytes part))
             ((vector (unsigned-byte 8)) part))))
    (remove-if (lambda (octets) (or (null octets) (zerop (length octets))))
               (if (listp body)
                   (mapcar #'chunk body)
                   (list (chunk body))))))

(defun response-header-present-p (headers key)
  (loop for k in headers by #'cddr
        thereis (or (eq k key)
                    (and (symbolp k)
                         (string-equal (symbol-name k) (symbol-name key)))
                    (and (stringp k)
                         (string-equal k (symbol-name key))))))

(defun prepare-response-body (headers body)
  "Return (values headers pending). A pathname body is not read here: its
   size goes in content-length and it is streamed as the window allows."
  (let ((pending (make-pending-response :end-stream t)))
    (cond
      ((pathnamep body)
       (let ((size (with-open-file (in body :element-type '(unsigned-byte 8))
                     (file-length in)))
             (headers (copy-list headers)))
         (unless (response-header-present-p headers :content-type)
           (setf (getf headers :content-type) (mimes:mime body)))
         (unless (response-header-present-p headers :content-length)
           (setf (getf headers :content-length) size))
         (setf (pending-path pending) body
               (pending-path-end pending) size)
         (values headers pending)))
      (t
       (dolist (octets (body-chunks body))
         (pending-append pending octets))
       (values headers pending)))))

(defun header-name-string (name)
  (string-downcase (etypecase name
                     (string name)
                     (symbol (symbol-name name)))))

(defun response-header-fields (status headers)
  "The response field list: :status, then HEADERS with lowercase names.
   Connection-specific fields, and fields the connection header names, are
   dropped: HTTP/2 must not carry them (RFC 9113 §8.2.2)."
  (let ((named-by-connection nil)
        (fields nil))
    (loop for (name value) on headers by #'cddr
          when (and value (string= (header-name-string name) "connection"))
            do (dolist (token (split-comma-list (princ-to-string value)))
                 (push (string-downcase token) named-by-connection)))
    (loop for (name value) on headers by #'cddr
          for name-str = (header-name-string name)
          unless (or (null value)
                     (member name-str *connection-specific-headers* :test #'string=)
                     (member name-str named-by-connection :test #'string=))
            do (push (cons name-str (princ-to-string value)) fields))
    (cons (cons ":status" (write-to-string status))
          (nreverse fields))))

(defun split-comma-list (string)
  (loop with start = 0
        for comma = (position #\, string :start start)
        for token = (string-trim '(#\Space #\Tab) (subseq string start comma))
        when (plusp (length token))
          collect token
        while comma
        do (setf start (1+ comma))))

(defun begin-response (conn stream status headers pending)
  "Send HEADERS, then as much of PENDING as the window allows. The rest
   stays queued on the stream. Returns T only when the response is complete."
  (let ((header-block (hpack-encode-headers
                       (http2-connection-encoder-context conn)
                       (response-header-fields status headers)))
        (done (and (pending-end-stream pending) (pending-empty-p pending))))
    (ensure-send-flush-hook conn)
    (unless (send-header-block conn stream header-block :end-stream done)
      (return-from begin-response nil))
    (cond
      (done
       (note-response-finished conn stream)
       t)
      (t
       (setf (gethash (http2-stream-id stream) (http2-connection-send-queue conn))
             pending)
       (advance-response conn stream)))))

(defun send-http2-response (conn stream status headers body)
  "Send HTTP/2 response on stream.
   HEADERS should be a plist of header names to values.
   BODY can be nil, a byte vector, a string, a list of strings/vectors, or a pathname.
   Returns T only when the response was fully written, including END_STREAM.
   NIL means the body tail is queued for a later WINDOW_UPDATE and was not
   dropped, or that the stream already has a response or is closed, in which
   case nothing is sent."
  (unless (response-startable-p conn stream)
    (return-from send-http2-response nil))
  (multiple-value-bind (headers pending) (prepare-response-body headers body)
    (begin-response conn stream status headers pending)))

(defun begin-streaming-response (conn stream status headers)
  "Send HEADERS without END_STREAM and queue an empty body for a writer.
   Returns the pending response, or NIL when refused."
  (when (response-startable-p conn stream)
    (let ((pending (make-pending-response :end-stream nil)))
      (begin-response conn stream status headers pending)
      (and (eq pending (gethash (http2-stream-id stream)
                                (http2-connection-send-queue conn)))
           pending))))

(defun streaming-write (conn stream pending octets close)
  "Queue OCTETS on the streaming response PENDING and send what fits.
   Dropped when PENDING is no longer the stream's response (reset, closed,
   or already finished)."
  (when (and (eq pending (gethash (http2-stream-id stream)
                                  (http2-connection-send-queue conn)))
             (not (pending-end-stream pending)))
    (pending-append pending octets)
    (when close
      (setf (pending-end-stream pending) t))
    (advance-response conn stream)))

(defun make-responder (conn socket stream run)
  "The Clack responder for a delayed response. A (status headers body)
   response is sent whole. A (status headers) response returns a writer,
   (lambda (data &key start end close)), like HTTP/1's streaming writer.
   Both may be called from any thread; RUN puts the work on the loop."
  (lambda (clack-res)
    (destructuring-bind (status headers &optional (body nil body-p)) clack-res
      (cond
        (body-p
         (funcall run
                  (lambda ()
                    (when (http2-socket-accepts-p socket)
                      (send-http2-response conn stream status headers body))))
         nil)
        (t
         (let ((pending nil))
           (funcall run
                    (lambda ()
                      (when (http2-socket-accepts-p socket)
                        (setf pending
                              (begin-streaming-response conn stream status headers)))))
           (lambda (data &key (start 0) end close)
             (let ((octets (octets-of data :start start :end end)))
               (funcall run
                        (lambda ()
                          (when (and pending (http2-socket-accepts-p socket))
                            (streaming-write conn stream pending octets close)))))
             nil)))))))

(defun dispatch-clack-response (conn socket stream response)
  "Send a Clack response. A function is a delayed response, same as HTTP/1."
  (etypecase response
    (cons
     (when (http2-socket-accepts-p socket)
       (destructuring-bind (status resp-headers &optional body) response
         (send-http2-response conn stream status resp-headers body))))
    (function
     (funcall response
              (make-responder conn socket stream (make-loop-runner socket))))))

(defun invoke-http2-app (conn socket stream app env)
  (handler-case
      (dispatch-clack-response conn socket stream (funcall app env))
    (error (e)
      (vom:error "Error in HTTP/2 app handler: ~A" e)
      (when (http2-socket-accepts-p socket)
        (cond
          ((response-startable-p conn stream)
           (send-http2-response conn stream 500
                                '(:content-type "text/plain")
                                "Internal Server Error"))
          ;; HEADERS are out and the body is not finished: a second
          ;; response is not allowed, so end the stream with RST_STREAM.
          ((gethash (http2-stream-id stream) (http2-connection-send-queue conn))
           (remhash (http2-stream-id stream) (http2-connection-send-queue conn))
           (connection-stream-error conn stream +internal-error+)))))))

(defun request-body-stream (octets)
  "An input stream over the request body, as HTTP/1 passes :raw-body."
  (flex:make-in-memory-input-stream octets :end (length octets)))

(defun respond-not-implemented (conn stream)
  "A request method we do not know. RFC 9110 §9.1: respond 501."
  (send-http2-response conn stream 501
                       '(:content-type "text/plain")
                       "Not Implemented"))

(defun run-request (conn socket stream app env body trailers)
  (cond
    ((null (getf env :request-method))
     (respond-not-implemented conn stream))
    (t
     (let ((body (or body (empty-octets))))
       (setf (getf env :http2.connection) conn
             (getf env :raw-body) (request-body-stream body)
             ;; Trailer fields as received, an alist of (name . value).
             (getf env :http2.trailers) trailers)
       ;; HTTP/2 has no chunked coding, so a body without content-length
       ;; gets its length here; Lack reads a body only when it is set.
       (when (and (plusp (length body))
                  (null (getf env :content-length)))
         (setf (getf env :content-length) (length body)))
       (invoke-http2-app conn socket stream app env)))))

(defun handle-http2-headers (conn socket stream headers end-stream app)
  "Validate, then run APP only for a complete request. Malformed headers RST first.
   A trailer section ends a request whose headers and body are already stored."
  (when (http2-stream-trailers-received stream)
    (return-from handle-http2-headers
      (handle-http2-data-end conn socket stream app)))
  (let ((env (build-clack-env socket stream headers)))
    (cond
      ((null env)
       (connection-stream-error conn stream +protocol-error+))
      (end-stream
       (run-request conn socket stream app env nil nil)))))

(defun handle-http2-data-end (conn socket stream app)
  (let* ((env (build-clack-env socket stream (http2-stream-headers stream)))
         (body (http2-stream-body-buffer stream)))
    (cond
      ((null env)
       (connection-stream-error conn stream +protocol-error+))
      (t
       (run-request conn socket stream app env body
                    (http2-stream-trailers stream))))))

(defun attach-http2-app (conn socket app)
  "Install the callbacks that run APP for each request on CONN. Returns CONN."
  (setf (http2-connection-on-headers conn)
        (lambda (stream headers end-stream)
          (handle-http2-headers conn socket stream headers end-stream app))

        (http2-connection-on-data conn)
        (lambda (stream data end-stream)
          (declare (ignore data))
          (when end-stream
            (handle-http2-data-end conn socket stream app)))

        (http2-connection-on-goaway conn)
        (lambda (last-stream-id error-code debug-data)
          (declare (ignore last-stream-id debug-data))
          (vom:info "Received GOAWAY with error code ~A" error-code))

        ;; Connection errors close the socket in the connection layer.
        (http2-connection-on-error conn)
        (lambda (error-code debug-data)
          (vom:error "HTTP/2 protocol error ~A: ~A"
                     error-code
                     (when debug-data
                       (trivial-utf-8:utf-8-bytes-to-string debug-data)))))
  conn)

(defun make-http2-app-handler (app)
  "Create HTTP/2 connection handler that invokes Clack app for each request.
   Returns a function that takes a socket and sets up HTTP/2 handling."
  (lambda (socket)
    (attach-http2-app (setup-http2-parser socket) socket app)))
