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
  (:import-from :woo.ssl)
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
           :*default-worker-num*))
(in-package :woo)

(defvar *default-backlog-size* 128)
(defvar *default-worker-num* nil)

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
               (<= backlog 128)))
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
        (ssl-context nil))
    (labels ((start-socket (socket)
               #-woo-no-ssl
               (when ssl
                 (woo.ssl:init-ssl-handle socket
                                          ssl-context
                                          ssl-cert-file
                                          ssl-key-file
                                          ssl-key-password))
               (setup-parser socket)
               (woo.ev.tcp:start-listening-socket socket))
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
      ;; Context ownership starts at allocation. Keep validation, ALPN setup,
      ;; and server startup in one cleanup scope so setup failures free it.
      (unwind-protect
           (progn
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
             #-woo-no-ssl
             (when ssl
               (setf ssl-context
                     (woo.ssl:create-context ssl-cert-file ssl-key-file ssl-key-password))
               ;; Keep ALPN configuration owned by this listener context.
               (woo.ssl:configure-context-alpn ssl-context woo.ssl:*alpn-protocols*))
             (if worker-num
                 (start-multithread-server)
                 (start-singlethread-server)))
        #-woo-no-ssl
        (when ssl-context
          (woo.ssl:free-context ssl-context))))))

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
  (let ((http (make-http-request))
        (body-buffer (make-smart-buffer)))
    (setf (wev:socket-data socket)
          (make-parser http
                       :body-callback
                       (lambda (data start end)
                         (declare (type (simple-array (unsigned-byte 8) (*)) data))
                         (if (smart-buffer::buffer-on-memory-p body-buffer)
                             (write-to-buffer body-buffer (subseq data start end) 0 (- end start))
                             (write-to-buffer body-buffer data start end)))
                       :finish-callback
                       (flet ((main (env)
                                (handle-response http socket
                                                 (if *debug*
                                                     (funcall *app* env)
                                                     (if-let (res (handler-case (funcall *app* env)
                                                                    (error (error)
                                                                      (vom:error (princ-to-string error))
                                                                      nil)))
                                                             res
                                                             '(500 nil nil)))
                                                 nil (eq (http-method http) :head))))
                         (lambda ()
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
                                   (main env)))))))))))

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

(defun handle-response (http socket clack-res &optional body-cleanup head-p)

  (let ((generation (woo.ev.socket::socket-response-generation socket)))
    (handler-case
        (etypecase clack-res
          (list (handle-normal-response http socket (validate-response clack-res head-p) body-cleanup head-p))
          (function
           (funcall clack-res
                    (lambda (response)
                      (progn
                        (handler-case
                            (handle-normal-response http socket (validate-response response head-p) body-cleanup head-p)
                          (error () (legacy-response-failed http socket generation))))))))
      (error () (legacy-response-failed http socket generation)))))

#+sbcl
(defun fd-file-size (fd)
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
