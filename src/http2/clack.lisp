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
           :build-clack-env
           :send-http2-response
           :validate-request-headers
           :combine-header-fields
           :connection-send-max-frame-size
           :*http2-frame-sink*))
(in-package :woo.http2.clack)

(defvar *http2-frame-sink* nil
  "When non-nil, a function of one frame argument invoked for each outbound frame.")

(defun emit-frame (conn frame)
  (when *http2-frame-sink*
    (funcall *http2-frame-sink* frame))
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

(defparameter *connection-specific-headers*
  '("connection" "keep-alive" "proxy-connection" "transfer-encoding" "upgrade"))

(defun connection-specific-header-p (name value)
  (let ((n (string-downcase name)))
    (or (member n *connection-specific-headers* :test #'string=)
        (and (string= n "te")
             (not (string-equal (string-trim '(#\Space #\Tab) value) "trailers"))))))

(defun request-pseudo-name-p (name)
  (member name '(":method" ":scheme" ":path" ":authority") :test #'string=))

(defun validate-request-headers (headers)
  "Return T if HEADERS is a valid HTTP/2 request header list, else NIL.
   Enforces pseudo-header order, required set, no response :status, no
   connection-specific fields (RFC 9113 §8.2–8.3)."
  (let ((seen-regular nil)
        (seen (make-hash-table :test 'equal)))
    (dolist (header headers)
      (let ((name (car header))
            (value (cdr header)))
        (unless (and (stringp name) (stringp value) (> (length name) 0))
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
             (return-from validate-request-headers nil)))
          (t
           (setf seen-regular t)
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
           (setf (getf env :request-method)
                 (intern (string-upcase value) :keyword)))
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
           (let ((colon-pos (position #\: value :from-end t)))
             (if (and colon-pos (> colon-pos 0))
                 (setf (getf env :server-name) (subseq value 0 colon-pos)
                       (getf env :server-port) (parse-integer value :start (1+ colon-pos) :junk-allowed t))
                 (setf (getf env :server-name) value))))
          ((string= name "content-type")
           (setf (getf env :content-type) value)
           (setf (gethash name http-headers) value))
          ((string= name "content-length")
           (setf (getf env :content-length) (parse-integer value :junk-allowed t))
           (setf (gethash name http-headers) value))
          ((string= name "host")
           (setf (gethash name http-headers) value)
           (unless (getf env :server-name)
             (let ((colon-pos (position #\: value :from-end t)))
               (if (and colon-pos (> colon-pos 0))
                   (setf (getf env :server-name) (subseq value 0 colon-pos)
                         (getf env :server-port) (parse-integer value :start (1+ colon-pos) :junk-allowed t))
                   (setf (getf env :server-name) value)))))
          ((not (pseudo-header-p name))
           (setf (gethash name http-headers) value)))))

    (unless (getf env :server-port)
      (setf (getf env :server-port)
            (if (string= (getf env :url-scheme) "https") 443 80)))
    (unless (getf env :path-info)
      (setf (getf env :path-info) "/"))
    (unless (getf env :request-uri)
      (setf (getf env :request-uri) (getf env :path-info)))

    (setf (getf env :headers) http-headers)
    env))

(defun send-header-block (conn stream-id header-block &key end-stream)
  "Send HEADER-BLOCK as HEADERS plus CONTINUATION frames at max frame size."
  (let* ((max (connection-send-max-frame-size conn))
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
        (when last (return))))))

(defun send-window-available (conn stream)
  (max 0 (min (http2-connection-remote-window-size conn)
              (http2-stream-window-size stream))))

(defun consume-send-window (conn stream n)
  (decf (http2-connection-remote-window-size conn) n)
  (decf (http2-stream-window-size stream) n))

(defun send-data-bytes (conn stream bytes &key end-stream)
  "Send BODY bytes as DATA frames split at max frame size, limited by send windows."
  (let* ((stream-id (http2-stream-id stream))
         (max (connection-send-max-frame-size conn))
         (len (length bytes))
         (offset 0))
    (when (and (zerop len) end-stream)
      (emit-frame conn (make-data-frame stream-id #() :end-stream t))
      (return-from send-data-bytes t))
    (loop while (< offset len)
          do (let* ((window (send-window-available conn stream))
                    (chunk (min max window (- len offset))))
               (when (<= chunk 0)
                 (return-from send-data-bytes nil))
               (let* ((end (= (+ offset chunk) len))
                      (fragment (subseq bytes offset (+ offset chunk))))
                 (emit-frame conn
                             (make-data-frame stream-id fragment
                                              :end-stream (and end-stream end)))
                 (consume-send-window conn stream chunk)
                 (incf offset chunk))))
    t))

(defun body-to-bytes (body)
  (etypecase body
    (null #())
    ((vector (unsigned-byte 8)) body)
    (string (string-to-utf-8-bytes body))
    (list
     (let ((parts (mapcar (lambda (chunk)
                            (etypecase chunk
                              (null #())
                              (string (string-to-utf-8-bytes chunk))
                              ((vector (unsigned-byte 8)) chunk)))
                          body)))
       (let* ((total (reduce #'+ parts :key #'length))
              (out (make-array total :element-type '(unsigned-byte 8)))
              (i 0))
         (dolist (p parts)
           (replace out p :start1 i)
           (incf i (length p)))
         out)))))

(defun send-http2-response (conn stream status headers body)
  "Send HTTP/2 response on stream.
   HEADERS should be a plist of header names to values.
   BODY can be nil, a byte vector, a string, or a list of strings/vectors."
  (let* ((status-str (write-to-string status))
         (response-headers (list (cons ":status" status-str))))

    (loop for (name value) on headers by #'cddr
          when value
          do (let ((name-str (etypecase name
                               (string (string-downcase name))
                               (keyword (string-downcase (symbol-name name)))
                               (symbol (string-downcase (symbol-name name))))))
               (push (cons name-str (princ-to-string value)) response-headers)))

    (setf response-headers (nreverse response-headers))

    (let* ((header-block (hpack-encode-headers
                          (http2-connection-encoder-context conn)
                          response-headers))
           (bytes (body-to-bytes body))
           (has-body (> (length bytes) 0)))
      (send-header-block conn (http2-stream-id stream) header-block
                         :end-stream (not has-body))
      (when has-body
        (send-data-bytes conn stream bytes :end-stream t)))))

(defun make-http2-app-handler (app)
  "Create HTTP/2 connection handler that invokes Clack app for each request.
   Returns a function that takes a socket and sets up HTTP/2 handling."
  (lambda (socket)
    (let ((conn nil))
      (setf conn
            (setup-http2-parser
             socket
             :on-headers
             (lambda (stream headers end-stream)
               (let ((env (build-clack-env socket stream headers)))
                 (cond
                   ((null env)
                    (connection-send-frame conn
                      (make-rst-stream-frame (http2-stream-id stream) +protocol-error+)))
                   (end-stream
                    (setf (getf env :http2.connection) conn
                          (getf env :raw-body) nil)
                    (handler-case
                        (let ((response (funcall app env)))
                          (when (and (listp response) (socket-open-p socket))
                            (destructuring-bind (status resp-headers &optional body) response
                              (send-http2-response conn stream status resp-headers body))))
                      (error (e)
                        (vom:error "Error in HTTP/2 app handler: ~A" e)
                        (when (socket-open-p socket)
                          (send-http2-response conn stream 500
                                               '(:content-type "text/plain")
                                               "Internal Server Error"))))))))

             :on-data
             (lambda (stream data end-stream)
               (declare (ignore data))
               (when end-stream
                 (let* ((env (build-clack-env socket stream (http2-stream-headers stream)))
                        (body (http2-stream-body-buffer stream)))
                   (cond
                     ((null env)
                      (connection-send-frame conn
                        (make-rst-stream-frame (http2-stream-id stream) +protocol-error+)))
                     (t
                      (setf (getf env :http2.connection) conn
                            (getf env :raw-body) (if (> (length body) 0) body nil))
                      (handler-case
                          (let ((response (funcall app env)))
                            (when (and (listp response) (socket-open-p socket))
                              (destructuring-bind (status resp-headers &optional resp-body) response
                                (send-http2-response conn stream status resp-headers resp-body))))
                        (error (e)
                          (vom:error "Error in HTTP/2 app handler: ~A" e)
                          (when (socket-open-p socket)
                            (send-http2-response conn stream 500
                                                 '(:content-type "text/plain")
                                                 "Internal Server Error"))))))))))

             :on-goaway
             (lambda (last-stream-id error-code debug-data)
               (declare (ignore last-stream-id debug-data))
               (vom:info "Received GOAWAY with error code ~A" error-code))

             :on-error
             (lambda (error-code debug-data)
               (vom:error "HTTP/2 protocol error ~A: ~A"
                          error-code
                          (when debug-data
                            (trivial-utf-8:utf-8-bytes-to-string debug-data)))))))))
