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

(defparameter *http-tchar-extra* "!#$%&'*+-.^_`|~"
  "tchar bytes that are not DIGIT or lowercase ALPHA (RFC 9110).")

(defun http-token-name-p (name)
  "HTTP/2 field names are lowercase tokens."
  (and (plusp (length name))
       (every (lambda (char)
                (or (char<= #\0 char #\9)
                    (char<= #\a char #\z)
                    (find char *http-tchar-extra* :test #'char=)))
              name)))

(defun field-value-ok-p (value)
  "RFC 9113 §8.2.1: no NUL, CR, or LF, and no leading or trailing SP/HTAB.
   An empty value is legal here; missing pseudo-header values are separate."
  (and (not (find-if (lambda (char)
                       (or (char= char #\Nul)
                           (char= char #\Return)
                           (char= char #\Newline)))
                     value))
       (or (zerop (length value))
           (let ((first (char value 0))
                 (last (char value (1- (length value)))))
             (flet ((ws (char)
                      (or (char= char #\Space) (char= char #\Tab))))
               (not (or (ws first) (ws last))))))))

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
           (apply-authority env value))
          ((string= name "content-type")
           (setf (getf env :content-type) value)
           (setf (gethash name http-headers) value))
          ((string= name "content-length")
           (setf (getf env :content-length) (parse-integer value :junk-allowed t))
           (setf (gethash name http-headers) value))
          ((string= name "host")
           (setf (gethash name http-headers) value)
           (unless (getf env :server-name)
             (apply-authority env value)))
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
  (multiple-value-bind (host port) (split-authority value)
    (setf (getf env :server-name) host)
    ;; Port 0 is a real port. Only NIL means "absent".
    (when (integerp port)
      (setf (getf env :server-port) port))))

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

(defun queue-unsent-data (conn stream bytes end-stream)
  "Hold bytes that do not fit in the send window. Returns NIL: not fully sent."
  (setf (gethash (http2-stream-id stream) (http2-connection-send-queue conn))
        (cons bytes (and end-stream t)))
  nil)

(defun send-data-bytes (conn stream bytes &key end-stream)
  "Send BODY bytes as DATA frames split at max frame size, limited by send windows.
   Bytes that do not fit are queued, not dropped. Returns T only when every byte
   has been written (END_STREAM on the last frame when requested). NIL means the
   tail is queued and must not be reported as a successful full send."
  (let* ((stream-id (http2-stream-id stream))
         (max (connection-send-max-frame-size conn))
         (len (length bytes))
         (offset 0))
    (when (and (zerop len) end-stream)
      (emit-frame conn (make-data-frame stream-id #() :end-stream t))
      (remhash stream-id (http2-connection-send-queue conn))
      (return-from send-data-bytes t))
    (loop while (< offset len)
          do (let* ((window (send-window-available conn stream))
                    (chunk (min max window (- len offset))))
               (when (<= chunk 0)
                 (return-from send-data-bytes
                   (queue-unsent-data conn stream (subseq bytes offset) end-stream)))
               (let* ((end (= (+ offset chunk) len))
                      (fragment (subseq bytes offset (+ offset chunk))))
                 (emit-frame conn
                             (make-data-frame stream-id fragment
                                              :end-stream (and end-stream end)))
                 (consume-send-window conn stream chunk)
                 (incf offset chunk))))
    (remhash stream-id (http2-connection-send-queue conn))
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

(defun read-pathname-octets (path)
  "Read a static-file body the way HTTP/1 sends a pathname response."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let* ((size (file-length in))
           (buf (make-array size :element-type '(unsigned-byte 8))))
      (read-sequence buf in)
      buf)))

(defun response-header-present-p (headers key)
  (loop for k in headers by #'cddr
        thereis (or (eq k key)
                    (and (symbolp k)
                         (string-equal (symbol-name k) (symbol-name key)))
                    (and (stringp k)
                         (string-equal k (symbol-name key))))))

(defun prepare-response-body (headers body)
  "Return (values headers bytes). Pathname bodies match HTTP/1 static files."
  (if (pathnamep body)
      (let ((bytes (read-pathname-octets body))
            (headers (copy-list headers)))
        (unless (response-header-present-p headers :content-type)
          (setf (getf headers :content-type) (mimes:mime body)))
        (unless (response-header-present-p headers :content-length)
          (setf (getf headers :content-length) (length bytes)))
        (values headers bytes))
      (values headers (body-to-bytes body))))

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

(defun ensure-send-flush-hook (conn)
  (unless (http2-connection-flush-sends conn)
    (setf (http2-connection-flush-sends conn) #'flush-pending-response-data)))

(defun flush-pending-response-data (conn &optional only-stream)
  "Write queued DATA now that a send window has grown. END_STREAM rides the last frame."
  (flet ((flush-one (stream)
           (let* ((id (http2-stream-id stream))
                  (entry (gethash id (http2-connection-send-queue conn))))
             (when (and entry (not (stream-closed-p stream)))
               (remhash id (http2-connection-send-queue conn))
               (when (send-data-bytes conn stream (car entry) :end-stream (cdr entry))
                 (note-response-finished conn stream))))))
    (if only-stream
        (flush-one only-stream)
        (dolist (id (let ((ids nil))
                      (maphash (lambda (id entry)
                                 (declare (ignore entry))
                                 (push id ids))
                               (http2-connection-send-queue conn))
                      (sort ids #'<)))
          (let ((stream (gethash id (http2-connection-streams conn))))
            (when stream
              (flush-one stream)))))))

(defun send-http2-response (conn stream status headers body)
  "Send HTTP/2 response on stream.
   HEADERS should be a plist of header names to values.
   BODY can be nil, a byte vector, a string, a list of strings/vectors, or a pathname.
   Returns T only when the response was fully written, including END_STREAM.
   NIL means the body tail is queued for a later WINDOW_UPDATE and was not dropped."
  (multiple-value-bind (headers bytes) (prepare-response-body headers body)
    (let* ((status-str (write-to-string status))
           (response-headers (list (cons ":status" status-str))))
      (loop for (name value) on headers by #'cddr
            unless (null value)
            do (let ((name-str (etypecase name
                                 (string (string-downcase name))
                                 (keyword (string-downcase (symbol-name name)))
                                 (symbol (string-downcase (symbol-name name))))))
                 (push (cons name-str (princ-to-string value)) response-headers)))
      (setf response-headers (nreverse response-headers))
      (let* ((header-block (hpack-encode-headers
                            (http2-connection-encoder-context conn)
                            response-headers))
             (has-body (> (length bytes) 0)))
        (ensure-send-flush-hook conn)
        (send-header-block conn (http2-stream-id stream) header-block
                           :end-stream (not has-body))
        (cond
          ((not has-body)
           (note-response-finished conn stream)
           t)
          ((send-data-bytes conn stream bytes :end-stream t)
           (note-response-finished conn stream)
           t)
          (t nil))))))

(defun http2-socket-accepts-p (socket)
  (or (null socket) (socket-open-p socket)))

(defun dispatch-clack-response (conn socket stream response)
  "Send a Clack response. A function is a delayed response, same as HTTP/1."
  (flet ((send-list (clack-res)
           (when (and (consp clack-res)
                      (http2-socket-accepts-p socket))
             (destructuring-bind (status resp-headers &optional body) clack-res
               (send-http2-response conn stream status resp-headers body)))))
    (etypecase response
      (cons (send-list response))
      (function (funcall response #'send-list)))))

(defun invoke-http2-app (conn socket stream app env)
  (handler-case
      (dispatch-clack-response conn socket stream (funcall app env))
    (error (e)
      (vom:error "Error in HTTP/2 app handler: ~A" e)
      (when (http2-socket-accepts-p socket)
        (send-http2-response conn stream 500
                             '(:content-type "text/plain")
                             "Internal Server Error")))))

(defun handle-http2-headers (conn socket stream headers end-stream app)
  "Validate, then run APP only for a complete request. Malformed headers RST first."
  (let ((env (build-clack-env socket stream headers)))
    (cond
      ((null env)
       (connection-stream-error conn stream +protocol-error+))
      (end-stream
       (setf (getf env :http2.connection) conn
             (getf env :raw-body) nil)
       (invoke-http2-app conn socket stream app env)))))

(defun handle-http2-data-end (conn socket stream app)
  (let* ((env (build-clack-env socket stream (http2-stream-headers stream)))
         (body (http2-stream-body-buffer stream)))
    (cond
      ((null env)
       (connection-stream-error conn stream +protocol-error+))
      (t
       (setf (getf env :http2.connection) conn
             (getf env :raw-body) (if (> (length body) 0) body nil))
       (invoke-http2-app conn socket stream app env)))))

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
               (handle-http2-headers conn socket stream headers end-stream app))

             :on-data
             (lambda (stream data end-stream)
               (declare (ignore data))
               (when end-stream
                 (handle-http2-data-end conn socket stream app)))

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
