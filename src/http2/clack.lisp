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
  (:import-from :woo.dispatch
                :*dispatch-drop-callback*
                :dispatch-work
                :make-dispatch-work
                :dispatch-work-p
                :dispatch-work-thunk
                :dispatch-work-on-drop
                :loop-dispatcher
                :make-loop-dispatcher
                :dispatcher-id
                :dispatcher-evloop
                :dispatcher-watcher
                :dispatcher-lock
                :dispatcher-thunks
                :dispatcher-budgets
                :dispatcher-stopped
                :dispatcher-cancel-budgets
                :*dispatchers*
                :*dispatchers-lock*
                :*dispatcher-counter*
                :find-dispatcher
                :drain-dispatcher
                :dispatcher-enqueue
                :stop-dispatcher
                :current-loop-dispatcher
                :http2-socket-accepts-p
                :dispatcher-runner
                :make-loop-runner)
  (:export :make-http2-app-handler
           :request-method-keyword
           :*pathname-chunk-size*
           :*pathname-body-open-hook*
           :pathname-response-error
           :pathname-response-error-status
           :*max-queued-response-bytes*
           :*max-connection-queued-response-bytes*
           :attach-http2-app
           :build-clack-env
           :send-http2-response
           :validate-request-headers
           :combine-header-fields
           :connection-send-max-frame-size
           ;; Defined in woo.http2.connection; re-exported for callers.
           :*http2-frame-sink*))
(in-package :woo.http2.clack)

(define-condition pathname-response-error (error)
  ((status :initarg :status :reader pathname-response-error-status)))

(defun pathname-permission-denied-p ()
  (let ((errno (ignore-errors (wsys:errno))))
    (and (integerp errno) (= errno wsys:EACCES))))

(defun pathname-error-status (errno)
  "Map the errno captured from a failed pathname operation to HTTP status."
  (cond ((= errno wsys:EACCES) 403)
        ((or (= errno wsys:ENOENT) (= errno wsys:ENOTDIR)) 404)
        ((= errno wsys:EISDIR) 403)
        (t 500)))

(defun pathname-response-status (path &optional errno)
  "Return the response status for a pathname that cannot be prepared.
Missing paths are 404, directories and permission failures are 403, and
unexpected filesystem failures are 500."
  (handler-case
      (if (integerp errno)
          (pathname-error-status errno)
          (cond
            ((uiop:directory-exists-p (uiop:ensure-directory-pathname path)) 403)
            ((probe-file path) nil)
            ((pathname-permission-denied-p) 403)
            (t 404)))
    (file-error ()
      (if (integerp errno)
          (pathname-error-status errno)
          500))
    (error () 500)))

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

;;; Sending responses

(defparameter *pathname-chunk-size* 16384
  "Octets read from a pathname body at a time. The file is read only as the
   send window allows, and it is not held open while waiting for credit.")

(defvar *pathname-body-open-hook* nil
  "When non-nil, called with each file stream opened to read a pathname body.")

(defparameter *max-queued-response-bytes* (* 8 1024 1024)
  "Most octets a streamed response may hold unsent: queued for the send
   window, plus written from another thread and not yet run on the loop.
   A write past it resets the stream (RST_STREAM INTERNAL_ERROR) and the
   writer drops later writes. Read on the event-loop thread.")

(defparameter *max-connection-queued-response-bytes* (* 64 1024 1024)
  "Most octets a connection's responses may hold queued for the send window
   or reserved by work waiting to run on the event loop.
   A streamed write that takes the connection past it resets that stream, as
   for *max-queued-response-bytes*. Read on the event-loop thread.")

;;; One lock protects both dispatcher reservations and snapshots of the loop's
;;; pending queues. Work owns its payload through the reservation, so reset and
;;; loop teardown can clear it even while its dispatch thunk is still queued.
(defstruct response-budget
  (lock (bt2:make-lock :name "woo HTTP/2 response budget"))
  (entries (make-hash-table)) adjuster)
(defstruct response-budget-entry budget id (queued 0) (reserved 0) tokens pending dead)
(defstruct response-reservation entry (bytes 0) payload (active t))

(defun cancel-budget-entry (entry)
  (let ((budget (response-budget-entry-budget entry)))
    (bt2:with-lock-held ((response-budget-lock budget))
      ;; Detach the owned copies before their capacity becomes reusable.
      ;; This helper does not reacquire the budget lock.
      (when (response-budget-entry-pending entry)
        (clear-pending-response (response-budget-entry-pending entry)))
      (setf (response-budget-entry-dead entry) t
            (response-budget-entry-pending entry) nil)
      (dolist (token (response-budget-entry-tokens entry))
        (setf (response-reservation-active token) nil
              (response-reservation-payload token) nil))
      (when (response-budget-adjuster budget)
        (funcall (response-budget-adjuster budget)
                 (- (+ (response-budget-entry-queued entry) (response-budget-entry-reserved entry))) nil))
      (setf (response-budget-entry-tokens entry) nil
            (response-budget-entry-queued entry) 0
            (response-budget-entry-reserved entry) 0)
      (remhash (response-budget-entry-id entry) (response-budget-entries budget)))))

(defun cancel-response-budget (budget)
  (let ((entries (bt2:with-lock-held ((response-budget-lock budget))
                   (alexandria:hash-table-values (response-budget-entries budget)))))
    (dolist (entry entries) (cancel-budget-entry entry))))

(defun response-budget-entry-for (conn stream)
  "Called on the event loop, before a responder escapes to other threads."
  (let ((budget (or (woo.http2.connection::http2-connection-response-budget conn)
                    (setf (woo.http2.connection::http2-connection-response-budget conn)
                          (make-response-budget :adjuster (woo.http2.connection::http2-connection-budget-adjuster conn))))))
    (unless (woo.http2.connection::http2-connection-cancel-responses conn)
      (setf (woo.http2.connection::http2-connection-cancel-responses conn)
            (lambda (id)
              (let ((entries
                      (bt2:with-lock-held ((response-budget-lock budget))
                        (if id
                            (let ((entry (gethash id (response-budget-entries budget))))
                              (when entry (list entry)))
                            (alexandria:hash-table-values (response-budget-entries budget))))))
                (dolist (entry entries) (cancel-budget-entry entry)))))
      ;; Register once with the owner loop. The dispatcher's weak table does
      ;; not retain completed connections, but teardown cancels survivors.
      (let ((dispatcher (current-loop-dispatcher)))
        (when dispatcher
          (setf (dispatcher-cancel-budgets dispatcher) #'cancel-response-budget)
          (setf (gethash budget (dispatcher-budgets dispatcher)) t))))
    (bt2:with-lock-held ((response-budget-lock budget))
      (or (gethash (http2-stream-id stream) (response-budget-entries budget))
          (setf (gethash (http2-stream-id stream) (response-budget-entries budget))
                (make-response-budget-entry :budget budget :id (http2-stream-id stream)))))))

(defun reserve-response-bytes (entry bytes stream-limit connection-limit &key partial)
  (let ((budget (response-budget-entry-budget entry)))
    (bt2:with-lock-held ((response-budget-lock budget))
      (let ((total 0))
        (maphash (lambda (id other)
                   (declare (ignore id))
                   (incf total (+ (response-budget-entry-queued other)
                                 (response-budget-entry-reserved other))))
                 (response-budget-entries budget))
        (when partial
          (setf bytes (min bytes
                           (- stream-limit (response-budget-entry-queued entry)
                              (response-budget-entry-reserved entry))
                           (- connection-limit total))))
        (unless (or (response-budget-entry-dead entry)
                    (and partial (<= bytes 0))
                    (> (+ bytes (response-budget-entry-queued entry)
                          (response-budget-entry-reserved entry)) stream-limit)
                    (> (+ bytes total) connection-limit))
          (when (and (response-budget-adjuster budget)
                     (not (funcall (response-budget-adjuster budget) bytes t)))
            (return-from reserve-response-bytes nil))
          (let ((token (make-response-reservation :entry entry :bytes bytes)))
            (incf (response-budget-entry-reserved entry) bytes)
            (push token (response-budget-entry-tokens entry))
            token))))))

(defun release-response-reservation (token)
  (let* ((entry (response-reservation-entry token))
         (budget (response-budget-entry-budget entry)))
    (bt2:with-lock-held ((response-budget-lock budget))
      (when (response-reservation-active token)
        (when (response-budget-adjuster budget)
          (funcall (response-budget-adjuster budget) (- (response-reservation-bytes token)) nil))
        (decf (response-budget-entry-reserved entry) (response-reservation-bytes token))
        (setf (response-budget-entry-tokens entry)
              (delete token (response-budget-entry-tokens entry))
              (response-reservation-active token) nil
              (response-reservation-payload token) nil)))))

(defun response-budget-entry-dead-p (entry)
  (bt2:with-lock-held ((response-budget-lock (response-budget-entry-budget entry)))
    (response-budget-entry-dead entry)))

(defun install-reservation-payload (token payload &key copy)
  "Install PAYLOAD, or call its copy thunk while the reservation is protected.
   Cancellation cannot return the capacity while the producer owns an
   unfinished copy. Inactive reservations do not call the copy thunk."
  (let ((budget (response-budget-entry-budget (response-reservation-entry token))))
    (bt2:with-lock-held ((response-budget-lock budget))
      (when (response-reservation-active token)
        (setf (response-reservation-payload token) (if copy (funcall payload) payload))
        t))))

(defun call-with-response-reservation (run token thunk)
  "THUNK receives the reserved payload on the owner loop. Always release it."
  (let ((*dispatch-drop-callback*
          (lambda () (cancel-budget-entry (response-reservation-entry token)))))
    (if (funcall run
                     (lambda ()
                       (unwind-protect
                            (when (response-reservation-active token)
                              (funcall thunk (response-reservation-payload token)))
                         (release-response-reservation token))))
        t
        (progn (release-response-reservation token) nil))))

(defun response-octet-length (data &key (start 0) end)
  "Size without allocating a copy; used before reserving dispatcher storage."
  (etypecase data
    (null 0)
    (pathname 0)
    (string
     (loop for i from start below (or end (length data))
           for code = (char-code (char data i))
           sum (cond ((< code #x80) 1) ((< code #x800) 2)
                     ((< code #x10000) 3) (t 4))))
    ((vector (unsigned-byte 8)) (- (or end (length data)) start))
    (list (loop for part in data sum (response-octet-length part)))))

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
  ;; Octets in CHUNKS not yet sent.
  (queued 0 :type integer)
  ;; Shared accounting for this pending queue and dispatcher work.
  (budget-entry nil)
  ;; Retain caller-owned parts; only the budgeted copied slice is queued.
  (source nil :type list)
  (source-offset 0 :type fixnum)
  ;; UTF-8 byte offset inside the current source character, for tiny budgets.
  (source-byte-offset 0 :type (integer 0 3))
  ;; A pathname body, sent from PATH-OFFSET up to PATH-END.
  (path nil)
  (path-offset 0 :type integer)
  (path-end 0 :type integer)
  ;; The file's identity when the response began (open-file-identity). The
  ;; file is reopened for each send, and must still be the same file.
  (path-identity nil)
  ;; END_STREAM follows the octets above. NIL while a writer may add more.
  (end-stream nil))

(defun update-pending-budget (pending)
  (let ((entry (pending-budget-entry pending)))
    (when entry
      (bt2:with-lock-held ((response-budget-lock (response-budget-entry-budget entry)))
        (unless (response-budget-entry-dead entry)
          (let ((delta (- (pending-queued pending) (response-budget-entry-queued entry))))
            (when (response-budget-adjuster (response-budget-entry-budget entry))
              (unless (funcall (response-budget-adjuster (response-budget-entry-budget entry)) delta (plusp delta))
                (error "HTTP/2 output budget exceeded")))
            (setf (response-budget-entry-queued entry) (pending-queued pending))))))))

(defun pending-path-done-p (pending)
  (or (null (pending-path pending))
      (>= (pending-path-offset pending) (pending-path-end pending))))

(defun pending-empty-p (pending)
  (and (null (pending-chunks pending))
       (null (pending-source pending))
       (pending-path-done-p pending)))

(defun pending-append (pending octets)
  (when (plusp (length octets))
    (incf (pending-queued pending) (length octets))
    (let ((cell (list octets)))
      (if (pending-chunks pending)
          (setf (cdr (pending-tail pending)) cell)
          (setf (pending-chunks pending) cell))
      (setf (pending-tail pending) cell)
      (update-pending-budget pending))))

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
                                            (null (pending-source pending))
                                            (pending-path-done-p pending)
                                            (pending-end-stream pending))))
                            (emit-frame conn
                                        (make-data-frame id
                                                         (if (and (zerop start) (= end len))
                                                             chunk
                                                             (subseq chunk start end))
                                                         :end-stream last))
                            (consume-send-window conn stream n)
                            (decf (pending-queued pending) n)
                            (update-pending-budget pending)
                            (setf (pending-offset pending) end)
                            (when last
                              (return-from send-pending-chunks :finished)))))
               (pop (pending-chunks pending))
               (setf (pending-offset pending) 0)
               (unless (pending-chunks pending)
                 (setf (pending-tail pending) nil))))
    nil))

(defun utf8-code-width (code)
  (cond ((< code #x80) 1) ((< code #x800) 2) ((< code #x10000) 3) (t 4)))

(defun source-octets (pending capacity)
  "Copy at most CAPACITY bytes, even inside one UTF-8 character."
  (let* ((part (first (pending-source pending)))
         (start (pending-source-offset pending))
         (byte-start (pending-source-byte-offset pending)))
    (if (stringp part)
        (let ((end start) (byte-end byte-start) (n 0))
          ;; Find the exact allocation size in one bounded scan.
          (loop while (and (< end (length part)) (< n capacity))
                for width = (utf8-code-width (char-code (char part end)))
                for take = (min (- width byte-end) (- capacity n))
                do (incf n take) (incf byte-end take)
                   (when (= byte-end width) (incf end) (setf byte-end 0)))
          (let ((out (make-array n :element-type '(unsigned-byte 8)))
                (pos start) (byte byte-start))
            (dotimes (i n)
              (let* ((code (char-code (char part pos)))
                     (width (utf8-code-width code)))
                (setf (aref out i)
                      (if (= width 1) code
                          (if (zerop byte)
                              (logior (ecase width (2 #xC0) (3 #xE0) (4 #xF0))
                                      (ash code (- (* 6 (1- width)))))
                              (logior #x80
                                      (logand #x3F (ash code (- (* 6 (- width byte 1)))))))))
                (incf byte)
                (when (= byte width) (incf pos) (setf byte 0))))
            (setf (pending-source-offset pending) end
                  (pending-source-byte-offset pending) byte-end)
            out))
        (let* ((end (min (length part) (+ start capacity)))
               (out (octets-of part :start start :end end)))
          (setf (pending-source-offset pending) end)
          out))))

(defun send-pending-source (conn stream pending)
  "Stage only a reserved copy. A full copy budget resets the response, as
   for a streaming writer; it cannot leave a budget-only waiter stranded."
  (let ((max (connection-send-max-frame-size conn)))
    (loop while (pending-source pending)
          do (when (<= (send-window-available conn stream) 0)
               (return-from send-pending-source :blocked))
             (let* ((part (first (pending-source pending)))
                    (start (pending-source-offset pending)))
               (if (= start (length part))
                   (setf (pending-source pending) (rest (pending-source pending))
                         (pending-source-offset pending) 0
                         (pending-source-byte-offset pending) 0)
                   (let* ((requested (min max (send-window-available conn stream)
                                          (- (* (- (length part) start)
                                                (if (stringp part) 4 1))
                                             (pending-source-byte-offset pending))))
                          (token (reserve-response-bytes
                                  (pending-budget-entry pending) requested
                                  *max-queued-response-bytes*
                                  *max-connection-queued-response-bytes* :partial t)))
                     (unless token (return-from send-pending-source :failed))
                     (unwind-protect
                          (progn
                            (pending-append pending
                                            (source-octets pending (response-reservation-bytes token)))
                            ;; Pending accounting now owns the bytes. Releasing
                            ;; first would let another producer spend them twice.
                            (release-response-reservation token)
                            (when (= (pending-source-offset pending) (length part))
                              (setf (pending-source pending) (rest (pending-source pending))
                                    (pending-source-offset pending) 0
                                    (pending-source-byte-offset pending) 0))
                            (let ((result (send-pending-chunks conn stream pending)))
                              (when result (return-from send-pending-source result))))
                       (release-response-reservation token))))))
    nil))

(defun open-file-identity (in)
  "What identifies the file open on IN, compared with EQUAL. On SBCL its
   inode, device, size and mtime, from the open descriptor, so a file
   renamed over the path or grown in place does not match. Elsewhere its
   write date and length."
  #+sbcl
  (let ((st (sb-posix:fstat (sb-sys:fd-stream-fd in))))
    (list (sb-posix:stat-ino st)
          (sb-posix:stat-dev st)
          (sb-posix:stat-size st)
          (sb-posix:stat-mtime st)))
  #-sbcl
  (list (file-write-date in) (file-length in)))

(defun send-pending-path (conn stream pending)
  "Send the pathname body as the window allows, reading at most
   *pathname-chunk-size* octets at a time. The file is open only during this
   call. Returns :finished, :blocked, :failed (the file was replaced or
   changed since the response began, got shorter, or can no longer be
   opened or read), or NIL."
  (when (pending-path-done-p pending)
    (return-from send-pending-path nil))
  (when (<= (send-window-available conn stream) 0)
    (return-from send-pending-path :blocked))
  (let ((id (http2-stream-id stream))
        (max (connection-send-max-frame-size conn)))
    (handler-case
     (with-open-file (in (pending-path pending) :element-type '(unsigned-byte 8))
      (when *pathname-body-open-hook*
        (funcall *pathname-body-open-hook* in))
      ;; Octets from another file, or from a changed one, would reach the
      ;; peer as one body under the content-length already sent.
      (unless (equal (open-file-identity in) (pending-path-identity pending))
        (vom:error "HTTP/2 pathname body ~A changed while being sent"
                   (pending-path pending))
        (return-from send-pending-path :failed))
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
      ;; Deleted, replaced by something unreadable, or an I/O error.
      ((or file-error stream-error) (e)
        (vom:error "HTTP/2 pathname body ~A: ~A" (pending-path pending) e)
        (return-from send-pending-path :failed)))
    nil))

(defun pump-response (conn stream pending)
  "Send what the window allows. Returns :finished once END_STREAM is sent."
  (let ((result (send-pending-chunks conn stream pending)))
    (when result
      (return-from pump-response result)))
  (let ((result (send-pending-source conn stream pending)))
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

(defun clear-pending-response (pending)
  "Detach response data. Caller holds its budget lock, when it has one."
  (setf (pending-chunks pending) nil
        (pending-tail pending) nil
        (pending-offset pending) 0
        (pending-queued pending) 0
        (pending-source pending) nil
        (pending-source-offset pending) 0
        (pending-source-byte-offset pending) 0))

(defun discard-pending (pending)
  "Detach owned copies and release their capacity in the same critical section."
  (let ((entry (pending-budget-entry pending)))
    (if entry
        (bt2:with-lock-held ((response-budget-lock (response-budget-entry-budget entry)))
          (let ((adjuster (response-budget-adjuster (response-budget-entry-budget entry))))
            (when adjuster (funcall adjuster (- (response-budget-entry-queued entry)) nil)))
          (clear-pending-response pending)
          (setf (response-budget-entry-queued entry) 0))
        (clear-pending-response pending))))

(defun drop-pending-response (conn stream)
  "Remove STREAM's entry from the send queue and discard its octets."
  (let* ((queue (http2-connection-send-queue conn))
         (pending (gethash (http2-stream-id stream) queue)))
    (when pending
      (discard-pending pending)
      (remhash (http2-stream-id stream) queue))))

(defun reset-response (conn stream)
  "Abandon STREAM's unfinished response: drop its queue entry and end the
   stream with RST_STREAM INTERNAL_ERROR, since HEADERS are already out."
  (drop-pending-response conn stream)
  (connection-stream-error conn stream +internal-error+))

(defun advance-response (conn stream)
  "Send what the window allows of STREAM's pending response. Returns T once
   the response is complete. A reset or closed stream just loses its entry."
  (let* ((queue (http2-connection-send-queue conn))
         (id (http2-stream-id stream))
         (pending (gethash id queue)))
    (cond
      ((null pending) nil)
      ((not (stream-sendable-p conn stream))
       (drop-pending-response conn stream)
       nil)
      (t
       (ecase (pump-response conn stream pending)
         (:finished
          (discard-pending pending)
          (remhash id queue)
          (note-response-finished conn stream)
          t)
         (:failed
          (vom:error "HTTP/2 response on stream ~D could not be sent within its resource limits"
                     id)
          (reset-response conn stream)
          nil)
         ((:blocked nil) nil))))))

(defun ensure-send-flush-hook (conn)
  (unless (http2-connection-flush-sends conn)
    (setf (http2-connection-flush-sends conn) #'flush-pending-response-data))
  (unless (http2-connection-discard-sends conn)
    (setf (http2-connection-discard-sends conn) #'discard-pending)))

(defun advance-response-guarded (conn stream)
  "advance-response, but an error resets STREAM instead of escaping, so its
   queue entry is not retried on every WINDOW_UPDATE and the other queued
   streams are still served."
  (handler-case (advance-response conn stream)
    (error (e)
      (vom:error "Error sending HTTP/2 response on stream ~D: ~A"
                 (http2-stream-id stream) e)
      (handler-case (reset-response conn stream)
        (error (e)
          (vom:error "Error resetting HTTP/2 stream ~D: ~A"
                     (http2-stream-id stream) e)))
      nil)))

(defun flush-pending-response-data (conn &optional only-stream)
  "Write queued DATA now that a send window has grown. END_STREAM rides the last frame."
  (if only-stream
      (advance-response-guarded conn only-stream)
      (dolist (id (let ((ids nil))
                    (maphash (lambda (id entry)
                               (declare (ignore entry))
                               (push id ids))
                             (http2-connection-send-queue conn))
                    (sort ids #'<)))
        (let ((stream (gethash id (http2-connection-streams conn))))
          (if stream
              (advance-response-guarded conn stream)
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

(defun response-header-present-p (headers key)
  (loop for k in headers by #'cddr
        thereis (or (eq k key)
                    (and (symbolp k)
                         (string-equal (symbol-name k) (symbol-name key)))
                    (and (stringp k)
                         (string-equal k (symbol-name key))))))

(defun prepare-response-body (conn headers body)
  "Return (values headers pending). Ordinary body parts are retained until
   sent, with only frame-sized copies made when the send window permits."
  (declare (ignore conn))
  (let ((pending (make-pending-response :end-stream t)))
    (cond
      ((pathnamep body)
       (let ((preflight-status (pathname-response-status body)))
         (when preflight-status
           (error 'pathname-response-error :status preflight-status))
         (multiple-value-bind (size identity)
             (handler-case
                 (with-open-file (in body :element-type '(unsigned-byte 8))
                   (values (file-length in) (open-file-identity in)))
               (file-error ()
                 (let ((errno (ignore-errors (wsys:errno))))
                   (error 'pathname-response-error
                          :status (pathname-response-status body errno))))
               (error ()
                 (error 'pathname-response-error :status 500)))
         (let ((headers (copy-list headers)))
           (unless (response-header-present-p headers :content-type)
             (setf (getf headers :content-type) (mimes:mime body)))
           (unless (response-header-present-p headers :content-length)
             (setf (getf headers :content-length) size))
           (setf (pending-path pending) body
                 (pending-path-end pending) size
                 (pending-path-identity pending) identity)
           (values headers pending)))))
      (t
       (setf (pending-source pending)
             (remove-if (lambda (part)
                          (or (null part)
                              (zerop (length (etypecase part
                                               (string part)
                                               ((vector (unsigned-byte 8)) part))))))
                        (if (listp body) body (list body))))
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
    (let ((entry (response-budget-entry-for conn stream)))
      (setf (pending-budget-entry pending) entry)
      (bt2:with-lock-held ((response-budget-lock (response-budget-entry-budget entry)))
        (setf (response-budget-entry-pending entry) pending))
      (update-pending-budget pending))
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
  (handler-case
      (multiple-value-bind (headers pending) (prepare-response-body conn headers body)
        (if pending
            (begin-response conn stream status headers pending)
            (progn
              (connection-stream-error conn stream +internal-error+)
              nil)))
    (pathname-response-error (e)
      ;; Preparation happens before HEADERS are sent, so a filesystem error
      ;; can still be represented as an ordinary response. Once a pathname
      ;; body is queued, send-pending-path resets the stream instead.
      (send-http2-response conn stream
                           (pathname-response-error-status e)
                           '(:content-length 0)
                           nil))))

(defun begin-streaming-response (conn stream status headers)
  "Send HEADERS without END_STREAM and queue an empty body for a writer.
   Returns the pending response, or NIL when refused."
  (when (response-startable-p conn stream)
    (let ((pending (make-pending-response :end-stream nil)))
      (begin-response conn stream status headers pending)
      (and (eq pending (gethash (http2-stream-id stream)
                                (http2-connection-send-queue conn)))
           pending))))

(defun connection-queued-bytes (conn)
  "Octets held unsent by all of CONN's queued responses."
  (let ((total 0))
    (maphash (lambda (id pending)
               (declare (ignore id))
               (incf total (pending-queued pending)))
             (http2-connection-send-queue conn))
    total))

(defun streaming-write (conn stream pending octets close)
  "Queue OCTETS on the streaming response PENDING and send what fits.
   Returns T when the octets were taken. NIL when they were dropped because
   PENDING is no longer the stream's response (reset, closed, or already
   finished), or because what stays queued is over
   *max-queued-response-bytes* or *max-connection-queued-response-bytes*;
   then the stream is reset with INTERNAL_ERROR. Not CANCEL: the peer may
   keep its window closed and still want the response. The server gives up
   on a response it cannot finish, as for any other internal failure."
  (unless (eq pending (gethash (http2-stream-id stream)
                               (http2-connection-send-queue conn)))
    ;; The peer may have reset the stream, leaving octets only we hold.
    (discard-pending pending))
  (when (and (eq pending (gethash (http2-stream-id stream)
                                  (http2-connection-send-queue conn)))
             (not (pending-end-stream pending)))
    (pending-append pending octets)
    (when close
      (setf (pending-end-stream pending) t))
    (cond
      ((advance-response conn stream) t)
      ;; Reset while sending.
      ((not (eq pending (gethash (http2-stream-id stream)
                                 (http2-connection-send-queue conn))))
       nil)
      ((or (> (pending-queued pending) *max-queued-response-bytes*)
           (> (connection-queued-bytes conn) *max-connection-queued-response-bytes*))
       (vom:warn "HTTP/2 stream ~D: ~D response octets queued, over the limit; resetting"
                 (http2-stream-id stream) (pending-queued pending))
       (reset-response conn stream)
       nil)
      (t t))))

(defun abandon-response (conn socket stream)
  "After an error while responding on STREAM: a 500 response when nothing
   has been sent, else RST_STREAM INTERNAL_ERROR when HEADERS are out and
   the body is not finished (a second response is not allowed)."
  (when (http2-socket-accepts-p socket)
    (cond
      ((response-startable-p conn stream)
       (send-http2-response conn stream 500
                            '(:content-type "text/plain")
                            "Internal Server Error"))
      ((gethash (http2-stream-id stream) (http2-connection-send-queue conn))
       (reset-response conn stream)))))

(defmacro with-response-errors ((conn socket stream what) &body body)
  "Run BODY. An error is logged and the response abandoned (abandon-response)."
  (let ((e (gensym "E")))
    `(handler-case (progn ,@body)
       (error (,e)
         (vom:error "Error in HTTP/2 ~A: ~A" ,what ,e)
         (abandon-response ,conn ,socket ,stream)
         nil))))

(defun make-responder (conn socket stream run)
  "A delayed responder with connection-wide reservations before dispatch.
   A writer's T means accepted for dispatch; later socket/stream closure may
   still discard it. Ordinary bodies retain caller-owned source references;
   their bounded copies are accounted by the ordinary response pump."
  (let ((entry (response-budget-entry-for conn stream))
        (stream-limit *max-queued-response-bytes*)
        (connection-limit *max-connection-queued-response-bytes*))
    (labels ((reset ()
               (funcall run (lambda ()
                              (when (and (http2-socket-accepts-p socket)
                                         (stream-sendable-p conn stream))
                                (reset-response conn stream)))))
             (reserve (data &key (start 0) end)
               (reserve-response-bytes entry (response-octet-length data :start start :end end)
                                       stream-limit connection-limit)))
      (lambda (clack-res)
        (destructuring-bind (status headers &optional (body nil body-p)) clack-res
          (cond
            (body-p
             ;; Ordinary responses retain the application's source without
             ;; copying it. A cancellable zero-byte token owns that reference;
             ;; the ordinary pump accounts for any copies it makes later.
             (let ((token (reserve-response-bytes entry 0 stream-limit connection-limit)))
               (when token
                 (install-reservation-payload token clack-res)
                 (call-with-response-reservation
                  run token
                  (lambda (response)
                    (with-response-errors (conn socket stream "delayed response")
                      (when (http2-socket-accepts-p socket)
                        (destructuring-bind (status headers body) response
                          (send-http2-response conn stream status headers body))))))))
             nil)
            (t
             (let ((pending nil)
                   (lock (bt2:make-lock :name "woo HTTP/2 writer"))
                   (dead nil))
               (flet ((mark-dead () (bt2:with-lock-held (lock) (setf dead t))))
                 (unless (funcall run
                                  (lambda ()
                                    (with-response-errors (conn socket stream "delayed response")
                                      (when (http2-socket-accepts-p socket)
                                        (setf pending (begin-streaming-response conn stream status headers))))
                                    (unless pending (mark-dead))))
                   (mark-dead))
                 (lambda (data &key (start 0) end close)
                   (block nil
                   (when (bt2:with-lock-held (lock) dead)
                     (return-from nil nil))
                   (let ((token (reserve data :start start :end end)))
                     (unless token
                       (mark-dead)
                       ;; A reset or completed stream also refuses new
                       ;; reservations. It must not emit another RST_STREAM.
                       (unless (response-budget-entry-dead-p entry)
                         (reset))
                       (return-from nil nil))
                     ;; Do not allocate until the shared budget accepted this
                     ;; write. The thunk retains TOKEN, not DATA or OCTETS.
                     (handler-case
                         (install-reservation-payload
                          token (lambda () (octets-of data :start start :end end)) :copy t)
                       (error (e)
                         (release-response-reservation token)
                         (error e)))
                     (unless
                         (call-with-response-reservation
                          run token
                          (lambda (octets)
                            (unless (with-response-errors (conn socket stream "streaming response")
                                      (and pending (http2-socket-accepts-p socket)
                                           (streaming-write conn stream pending octets close)))
                              (mark-dead))))
                       (mark-dead))
                     (bt2:with-lock-held (lock) (not dead))))))))))))))

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
  (with-response-errors (conn socket stream "app handler")
    (dispatch-clack-response conn socket stream (funcall app env))))

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
