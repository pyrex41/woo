(in-package :cl-user)
(defpackage woo.http2.connection
  (:use :cl
        :woo.http2.constants
        :woo.http2.frames
        :woo.http2.hpack
        :woo.http2.stream)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes)
  (:import-from :woo.ev.socket
                :socket
                :socket-data
                :socket-open-p
                :write-socket-data
                :with-async-writing
                :close-socket)
  (:export :make-http2-connection
           :http2-connection
           :http2-connection-socket
           :http2-connection-streams
           :http2-connection-encoder-context
           :http2-connection-decoder-context
           :http2-connection-last-stream-id
           :http2-connection-goaway-sent
           :http2-connection-local-settings
           :http2-connection-remote-settings
           :http2-connection-window-size
           :connection-get-stream
           :connection-send-frame
           :connection-send-goaway
           :connection-stream-error
           :connection-drop-closed-stream
           :connection-process-frame
           :http2-connection-remote-window-size
           :http2-connection-remote-max-frame-size
           :http2-connection-local-max-frame-size
           :http2-connection-awaiting-continuation-stream-id
           :http2-connection-send-queue
           :http2-connection-flush-sends
           :http2-connection-on-headers
           :http2-connection-on-data
           :http2-connection-on-goaway
           :http2-connection-on-error
           :http2-connection-on-close
           :setup-http2-parser
           :*http2-frame-sink*))
(in-package :woo.http2.connection)

(defun default-local-settings ()
  "A fresh settings alist. A backquoted list of constants can be folded
   into one literal shared by every connection, so editing one would
   change them all."
  (list (cons +settings-max-concurrent-streams+ 100)
        (cons +settings-initial-window-size+ +default-initial-window-size+)
        (cons +settings-max-frame-size+ +default-max-frame-size+)
        (cons +settings-header-table-size+ +default-header-table-size+)
        (cons +settings-max-header-list-size+ +default-max-header-list-size+)))

(defstruct http2-connection
  "HTTP/2 connection state."
  socket
  (buffer (make-array 0 :element-type '(unsigned-byte 8)
                      :adjustable t :fill-pointer 0))
  (preface-received nil :type boolean)
  (settings-sent nil :type boolean)
  (settings-ack-received nil :type boolean)
  (encoder-context (make-hpack-context))
  (decoder-context (make-hpack-context))
  (streams (make-hash-table))
  (last-stream-id 0 :type (unsigned-byte 32))
  (next-push-stream-id 2 :type (unsigned-byte 32))  ; Server push uses even IDs
  (goaway-sent nil :type boolean)
  (goaway-received nil :type boolean)
  ;; (stream-id . error-code) of the most recent RST_STREAM we sent.
  (last-rst nil)
  ;; Connection-level flow control
  (window-size +default-initial-window-size+ :type integer)
  (remote-window-size +default-initial-window-size+ :type integer)
  ;; Settings
  (local-settings (default-local-settings))
  (remote-settings nil)
  (remote-max-frame-size +default-max-frame-size+ :type integer)
  (remote-initial-window-size +default-initial-window-size+ :type integer)
  (local-max-frame-size +default-max-frame-size+ :type integer)
  (local-max-concurrent-streams 100 :type integer)
  (awaiting-continuation-stream-id nil)
  ;; Set when the client preface is accepted. The next frame must be SETTINGS.
  (awaiting-first-settings nil :type boolean)
  ;; Closed streams are removed from STREAMS so they do not accumulate. Only
  ;; the ids of recently closed streams are kept (id -> T), bounded by
  ;; *closed-stream-retention*. Older client ids at or below last-stream-id
  ;; are closed by RFC 9113 §5.1.1, so they need no entry.
  (closed-streams (make-hash-table) :type hash-table)
  ;; Set once a connection error has queued GOAWAY and the socket close.
  (closing nil :type boolean)
  ;; stream-id -> (cons unsent-octets end-stream-p). Flushed on WINDOW_UPDATE.
  (send-queue (make-hash-table) :type hash-table)
  ;; (lambda (conn &optional stream)) installed by the response writer.
  (flush-sends nil)
  ;; Callbacks
  on-stream       ; (lambda (stream)) - called for new stream
  on-headers      ; (lambda (stream headers end-stream))
  on-data         ; (lambda (stream data end-stream))
  on-goaway       ; (lambda (last-stream-id error-code debug-data))
  on-error        ; (lambda (error-code debug-data))
  on-close)       ; (lambda (conn)) - connection error; socket closes after GOAWAY

(defparameter *closed-stream-retention* 128
  "Closed stream ids remembered exactly. When the table grows past this,
   the lower half is dropped; those ids stay closed by the §5.1.1 rule.")

(defun connection-stream-id-closed-p (conn stream-id)
  "True for an id that is not open and has been closed. A client id at or
   below last-stream-id was either used or implicitly closed when a higher
   id opened (RFC 9113 §5.1.1)."
  (or (gethash stream-id (http2-connection-closed-streams conn))
      (and (oddp stream-id)
           (<= stream-id (http2-connection-last-stream-id conn)))))

(defun connection-find-stream (conn stream-id)
  "Return the open stream, or a fresh closed placeholder for a closed id.
   Closed stream objects are not retained, so the placeholder carries only
   the id and the closed state."
  (or (gethash stream-id (http2-connection-streams conn))
      (when (and (plusp stream-id)
                 (connection-stream-id-closed-p conn stream-id))
        (make-http2-stream :id stream-id :state +state-closed+))))

(defun prune-closed-stream-ids (conn)
  "Drop the lower half of the remembered closed ids once over the bound."
  (let ((closed (http2-connection-closed-streams conn)))
    (when (> (hash-table-count closed) *closed-stream-retention*)
      (let ((ids (sort (loop for id being the hash-keys of closed collect id) #'<)))
        (loop repeat (- (length ids) (floor *closed-stream-retention* 2))
              for id in ids
              do (remhash id closed))))))

(defun connection-drop-closed-stream (conn stream)
  "Remove a closed stream from the open table. Idempotent.
   Keeps only the id, so later frames are not idle errors."
  (when (and stream (stream-closed-p stream))
    (let ((id (http2-stream-id stream)))
      (remhash id (http2-connection-streams conn))
      (remhash id (http2-connection-send-queue conn))
      (setf (gethash id (http2-connection-closed-streams conn)) t)
      (prune-closed-stream-ids conn)))
  stream)

(defun connection-get-stream (conn stream-id &key (create nil))
  "Get stream by ID, optionally creating it if it doesn't exist."
  (or (connection-find-stream conn stream-id)
      (when create
        (let ((stream (make-http2-stream
                       :id stream-id
                       :window-size (http2-connection-remote-initial-window-size conn)
                       :recv-window-size +default-initial-window-size+)))
          (setf (gethash stream-id (http2-connection-streams conn)) stream)
          (when (http2-connection-on-stream conn)
            (funcall (http2-connection-on-stream conn) stream))
          stream))))

(defun connection-flush-pending-sends (conn &optional stream)
  "Invoke the response writer's flush hook after a send window grows."
  (let ((fn (http2-connection-flush-sends conn)))
    (when fn
      (funcall fn conn stream))))

(defvar *http2-frame-sink* nil
  "When non-nil, a function of one frame argument invoked for each outbound frame.")

(defun connection-send-frame (conn frame)
  "Send a frame on the connection. Nothing is sent once the connection is
   closing: a later write would also clear the pending close callback."
  (unless (http2-connection-closing conn)
    (when *http2-frame-sink*
      (funcall *http2-frame-sink* frame))
    (let ((socket (http2-connection-socket conn)))
      (when (and socket (socket-open-p socket))
        (with-async-writing (socket)
          (write-socket-data socket (serialize-frame frame)))))))

(defun connection-send-goaway (conn error-code &optional debug-data)
  "Send GOAWAY frame and mark connection for shutdown."
  (unless (http2-connection-goaway-sent conn)
    (setf (http2-connection-goaway-sent conn) t)
    (connection-send-frame conn
      (make-goaway-frame (http2-connection-last-stream-id conn)
                         error-code
                         debug-data))))

(defun connection-close (conn)
  "Close the connection after a connection error. The socket is closed by
   the write callback, so the queued GOAWAY is flushed first. Idempotent."
  (unless (http2-connection-closing conn)
    (setf (http2-connection-closing conn) t)
    (when (http2-connection-on-close conn)
      (funcall (http2-connection-on-close conn) conn))
    (let ((socket (http2-connection-socket conn)))
      (when (and socket (socket-open-p socket))
        (with-async-writing (socket :write-cb #'close-socket))))))

;;; Frame handlers

(defun connection-protocol-error (conn error-code &optional debug-data)
  "Record a connection error, send GOAWAY, invoke on-error, then close."
  (connection-send-goaway conn error-code debug-data)
  (when (http2-connection-on-error conn)
    (funcall (http2-connection-on-error conn) error-code debug-data))
  (connection-close conn)
  nil)

(defun connection-stream-error (conn stream error-code)
  "Record a stream error, send RST_STREAM, invoke on-error."
  (when stream
    (setf (http2-connection-last-rst conn)
          (cons (http2-stream-id stream) error-code))
    (unless (stream-closed-p stream)
      (stream-transition stream :send-rst))
    (connection-send-frame conn
      (make-rst-stream-frame (http2-stream-id stream) error-code))
    (connection-drop-closed-stream conn stream))
  (when (http2-connection-on-error conn)
    (funcall (http2-connection-on-error conn) error-code nil))
  nil)

(defun connection-open-stream-count (conn)
  (let ((n 0))
    (maphash (lambda (id stream)
               (declare (ignore id))
               (let ((state (http2-stream-state stream)))
                 (unless (or (= state +state-idle+)
                             (= state +state-closed+))
                   (incf n))))
             (http2-connection-streams conn))
    n))

(defun unpadded-payload (payload flags)
  "Return (values data error-code). error-code is set on illegal padding."
  (if (logbitp 3 flags)                     ; PADDED
      (if (zerop (length payload))
          (values nil +protocol-error+)
          (let ((pad-length (aref payload 0)))
            (if (>= pad-length (length payload))
                (values nil +protocol-error+)
                (values (subseq payload 1 (- (length payload) pad-length)) nil))))
      (values payload nil)))

(defun handle-settings-frame (conn frame)
  "Handle received SETTINGS frame."
  (unless (zerop (frame-stream-id frame))
    (return-from handle-settings-frame
      (connection-protocol-error conn +protocol-error+)))
  (let* ((payload (frame-payload frame))
         (ack (logbitp 0 (frame-flags frame))))
    (cond
      (ack
       (unless (zerop (length payload))
         (return-from handle-settings-frame
           (connection-protocol-error conn +frame-size-error+)))
       (setf (http2-connection-settings-ack-received conn) t))
      ((not (zerop (mod (length payload) 6)))
       (connection-protocol-error conn +frame-size-error+))
      (t
       (let ((settings (parse-settings-payload payload)))
         (dolist (setting settings)
           (let ((id (car setting))
                 (value (cdr setting)))
             (case id
               (#.+settings-header-table-size+
                (hpack-context-update-size
                 (http2-connection-encoder-context conn) value))
               (#.+settings-enable-push+
                ;; Any value other than 0 or 1 is a connection error.
                (unless (or (= value 0) (= value 1))
                  (return-from handle-settings-frame
                    (connection-protocol-error conn +protocol-error+))))
               (#.+settings-max-frame-size+
                (unless (and (>= value +min-max-frame-size+)
                             (<= value +max-frame-size-limit+))
                  (return-from handle-settings-frame
                    (connection-protocol-error conn +protocol-error+)))
                (setf (http2-connection-remote-max-frame-size conn) value))
               (#.+settings-initial-window-size+
                (when (> value +max-window-size+)
                  (return-from handle-settings-frame
                    (connection-protocol-error conn +flow-control-error+)))
                (let ((delta (- value (http2-connection-remote-initial-window-size conn))))
                  (maphash (lambda (id stream)
                             (declare (ignore id))
                             (let ((new (+ (http2-stream-window-size stream) delta)))
                               (when (> new +max-window-size+)
                                 (return-from handle-settings-frame
                                   (connection-protocol-error conn +flow-control-error+)))
                               (setf (http2-stream-window-size stream) new)))
                           (http2-connection-streams conn))
                  (setf (http2-connection-remote-initial-window-size conn) value)))))
           (push setting (http2-connection-remote-settings conn)))
         (connection-send-frame conn (make-settings-ack-frame))
         ;; INITIAL_WINDOW_SIZE may have unblocked queued DATA.
         (connection-flush-pending-sends conn))))))

(defun validate-new-stream-id (conn stream-id)
  "Reject illegal client HEADERS stream IDs. Returns T if ok."
  (cond
    ((zerop stream-id)
     (connection-protocol-error conn +protocol-error+)
     nil)
    ((evenp stream-id)
     (connection-protocol-error conn +protocol-error+)
     nil)
    (t
     ;; Only a stream we know about may take HEADERS again. An unused id
     ;; at or below last-stream-id is PROTOCOL_ERROR, not a closed stream.
     (let ((existing (or (gethash stream-id (http2-connection-streams conn))
                         (and (gethash stream-id (http2-connection-closed-streams conn))
                              (connection-find-stream conn stream-id)))))
       (cond
         ((null existing)
          (when (<= stream-id (http2-connection-last-stream-id conn))
            (connection-protocol-error conn +protocol-error+)
            (return-from validate-new-stream-id nil))
          ;; The id is consumed, but the header block is still decoded.
          ;; RST happens after that decode, not here.
          (when (>= (connection-open-stream-count conn)
                    (http2-connection-local-max-concurrent-streams conn))
            (setf (http2-connection-last-stream-id conn) stream-id)
            (let ((stream (make-http2-stream
                           :id stream-id
                           :window-size (http2-connection-remote-initial-window-size conn)
                           :recv-window-size +default-initial-window-size+
                           :refused t)))
              (setf (gethash stream-id (http2-connection-streams conn)) stream))
            (return-from validate-new-stream-id t))
          t)
         (t t))))))

(defun connection-header-list-limit (conn)
  "Enforced header-list cap. Same value we advertise in SETTINGS."
  (or (cdr (assoc +settings-max-header-list-size+
                  (http2-connection-local-settings conn)))
      +default-max-header-list-size+))

(defun header-block-too-large-p (conn size)
  (> size (connection-header-list-limit conn)))

(defun clear-header-continuation (conn stream)
  (when stream
    (setf (http2-stream-header-buffer stream) nil
          (http2-stream-awaiting-continuation stream) nil
          (http2-stream-pending-end-stream stream) nil))
  (setf (http2-connection-awaiting-continuation-stream-id conn) nil))

(defun reject-header-list-size (conn stream)
  "Oversized header block: GOAWAY and drop any partial continuation."
  (clear-header-continuation conn stream)
  (connection-protocol-error conn +enhance-your-calm+))

(defun uncompressed-header-list-size (headers)
  "Name octets + value octets + 32 per field (RFC 9113 §6.5.2)."
  (let ((n 0))
    (dolist (header headers n)
      (incf n (+ 32
                 (length (string-to-utf-8-bytes (car header)))
                 (length (string-to-utf-8-bytes (cdr header))))))))

(defun parse-content-length-token (value)
  "HTTP content-length is 1*DIGIT (RFC 9110). NIL if it is not."
  (when (and (stringp value)
             (plusp (length value))
             (every (lambda (c) (char<= #\0 c #\9)) value))
    (parse-integer value)))

(defun accept-content-length (stream headers)
  "Return T if content-length is absent or one acceptable value.
   Duplicate fields, a non-integer, or a value that disagrees with one
   already stored are stream errors. 0 is a real length, not absence."
  (let ((raw (loop for pair in headers
                   when (and (consp pair)
                             (stringp (car pair))
                             (string= (car pair) "content-length"))
                   collect (cdr pair))))
    (cond
      ((null raw) t)
      ((rest raw) nil)
      (t
       (let ((n (parse-content-length-token (car raw))))
         (cond
           ((null n) nil)
           ;; A second content-length is a duplicate even when the
           ;; integer matches, and a mismatch is also rejected.
           ((integerp (http2-stream-content-length stream))
            nil)
           (t
            (setf (http2-stream-content-length stream) n)
            t)))))))

(defun content-length-complete-error-p (stream)
  "At END_STREAM the declared length must equal bytes received.
   A stored 0 is a real length, not an absent header."
  (let ((declared (http2-stream-content-length stream)))
    (and (integerp declared)
         (/= (http2-stream-bytes-received stream) declared))))

(defun decode-header-block (conn header-block)
  "HPACK-decode HEADER-BLOCK with the header-list cap enforced per field.
   Returns (values headers ok). OK is NIL when the cap was crossed; decoding
   stopped there and the dynamic table is out of sync with the peer."
  (handler-case
      (values (hpack-decode-headers (http2-connection-decoder-context conn)
                                    header-block
                                    :max-header-list-size
                                    (connection-header-list-limit conn))
              t)
    (hpack-header-list-too-large ()
      (values nil nil))))

(defun pseudo-header-field-p (header)
  (let ((name (car header)))
    (and (stringp name)
         (plusp (length name))
         (char= (char name 0) #\:))))

(defun finish-request-headers (conn stream stream-id headers end-stream)
  (setf (http2-stream-headers stream) headers)
  (when (> stream-id (http2-connection-last-stream-id conn))
    (setf (http2-connection-last-stream-id conn) stream-id))
  (unless (accept-content-length stream headers)
    (connection-stream-error conn stream +protocol-error+)
    (return-from finish-request-headers nil))
  (when end-stream
    (when (content-length-complete-error-p stream)
      (connection-stream-error conn stream +protocol-error+)
      (return-from finish-request-headers nil))
    (stream-transition stream :recv-end-stream))
  (when (http2-connection-on-headers conn)
    (funcall (http2-connection-on-headers conn)
             stream headers end-stream))
  (connection-drop-closed-stream conn stream))

(defun finish-trailers (conn stream trailers end-stream)
  "A later HEADERS on an open stream is a trailer section (RFC 9113 §8.1).
   It must carry END_STREAM and no pseudo-headers, or the request is
   malformed. content-length frames the body and cannot be a trailer
   (RFC 9110 §6.5.1). The request headers are kept; trailers are stored apart."
  (when (or (not end-stream)
            (some #'pseudo-header-field-p trailers)
            (assoc "content-length" trailers :test #'equal))
    (connection-stream-error conn stream +protocol-error+)
    (return-from finish-trailers nil))
  (setf (http2-stream-trailers stream) trailers
        (http2-stream-trailers-received stream) t)
  (when (content-length-complete-error-p stream)
    (connection-stream-error conn stream +protocol-error+)
    (return-from finish-trailers nil))
  (stream-transition stream :recv-end-stream)
  (when (http2-connection-on-headers conn)
    (funcall (http2-connection-on-headers conn)
             stream trailers end-stream))
  (connection-drop-closed-stream conn stream))

(defun finish-header-block (conn stream stream-id header-block end-stream)
  (when (header-block-too-large-p conn (length header-block))
    (return-from finish-header-block
      (reject-header-list-size conn stream)))
  (clear-header-continuation conn stream)
  (multiple-value-bind (headers ok) (decode-header-block conn header-block)
    (unless ok
      (return-from finish-header-block
        (connection-protocol-error conn +enhance-your-calm+)))
    ;; Refused streams stay out of the state machine. :recv-headers on a
    ;; stream we then RST would be a connection error from closed.
    (when (http2-stream-refused stream)
      (connection-stream-error conn stream +refused-stream+)
      (return-from finish-header-block nil))
    (let* ((state (http2-stream-state stream))
           (trailers-p (or (= state +state-open+)
                           (= state +state-half-closed-local+))))
      ;; Signals for states that cannot take HEADERS.
      (stream-transition stream :recv-headers)
      (if trailers-p
          (finish-trailers conn stream headers end-stream)
          (finish-request-headers conn stream stream-id headers end-stream)))))

(defun handle-headers-frame (conn frame)
  "Handle received HEADERS frame."
  (let* ((stream-id (frame-stream-id frame))
         (flags (frame-flags frame))
         (end-headers (logbitp 2 flags))
         (end-stream (logbitp 0 flags)))
    (unless (validate-new-stream-id conn stream-id)
      (return-from handle-headers-frame))
    (multiple-value-bind (payload pad-error)
        (unpadded-payload (frame-payload frame) flags)
      (when pad-error
        (return-from handle-headers-frame
          (connection-protocol-error conn pad-error)))
      (let ((header-start 0)
            (header-end (length payload)))
        (when (logbitp 5 flags)             ; PRIORITY
          (when (< (length payload) 5)
            (return-from handle-headers-frame
              (connection-protocol-error conn +frame-size-error+)))
          (incf header-start 5))
        (when (> header-start header-end)
          (return-from handle-headers-frame
            (connection-protocol-error conn +protocol-error+)))
        (let ((header-block (subseq payload header-start header-end)))
          (when (header-block-too-large-p conn (length header-block))
            (return-from handle-headers-frame
              (connection-protocol-error conn +enhance-your-calm+)))
          (let ((stream (connection-get-stream conn stream-id :create t)))
            (if end-headers
                (finish-header-block conn stream stream-id header-block end-stream)
                (progn
                  (setf (http2-stream-header-buffer stream) header-block
                        (http2-stream-awaiting-continuation stream) t
                        (http2-stream-pending-end-stream stream) end-stream
                        (http2-connection-awaiting-continuation-stream-id conn)
                        stream-id)))))))))

(defun handle-continuation-frame (conn frame)
  "Handle received CONTINUATION frame."
  (let* ((stream-id (frame-stream-id frame))
         (awaiting (http2-connection-awaiting-continuation-stream-id conn))
         (stream (connection-find-stream conn stream-id)))
    (unless (and awaiting stream
                 (= awaiting stream-id)
                 (http2-stream-awaiting-continuation stream))
      (return-from handle-continuation-frame
        (connection-protocol-error conn +protocol-error+)))
    (let* ((payload (frame-payload frame))
           (end-headers (logbitp 2 (frame-flags frame)))
           (old-buffer (http2-stream-header-buffer stream))
           (total (+ (if old-buffer (length old-buffer) 0)
                     (length payload))))
      (when (header-block-too-large-p conn total)
        (return-from handle-continuation-frame
          (reject-header-list-size conn stream)))
      (let ((new-buffer (concatenate '(vector (unsigned-byte 8))
                                     old-buffer payload)))
        (if end-headers
            (finish-header-block conn stream stream-id new-buffer
                                 (http2-stream-pending-end-stream stream))
            (setf (http2-stream-header-buffer stream) new-buffer))))))

(defun replenish-connection-window (conn)
  "Return consumed DATA credit to the peer (RFC 9113 §6.9). A WINDOW_UPDATE
   is sent once half the window is used, restoring it to the initial size."
  (let ((window (http2-connection-window-size conn)))
    (when (<= window (floor +default-initial-window-size+ 2))
      (setf (http2-connection-window-size conn) +default-initial-window-size+)
      (connection-send-frame conn
        (make-window-update-frame 0 (- +default-initial-window-size+ window))))))

(defun replenish-stream-window (conn stream)
  "As replenish-connection-window, for a stream that can still receive DATA.
   A stream the peer has finished gets no more credit."
  (let ((window (http2-stream-recv-window-size stream))
        (state (http2-stream-state stream)))
    (when (and (or (= state +state-open+)
                   (= state +state-half-closed-local+))
               (<= window (floor +default-initial-window-size+ 2)))
      (setf (http2-stream-recv-window-size stream) +default-initial-window-size+)
      (connection-send-frame conn
        (make-window-update-frame (http2-stream-id stream)
                                  (- +default-initial-window-size+ window))))))

(defun handle-data-frame (conn frame)
  "Handle received DATA frame."
  (let* ((stream-id (frame-stream-id frame))
         (stream (connection-find-stream conn stream-id))
         (flags (frame-flags frame))
         (end-stream (logbitp 0 flags))
         (raw-len (length (frame-payload frame))))
    (when (zerop stream-id)
      (return-from handle-data-frame
        (connection-protocol-error conn +protocol-error+)))
    (unless stream
      (return-from handle-data-frame
        (connection-protocol-error conn +protocol-error+)))
    (multiple-value-bind (data pad-error)
        (unpadded-payload (frame-payload frame) flags)
      (when pad-error
        (return-from handle-data-frame
          (connection-protocol-error conn pad-error)))
      ;; Connection window is connection state. A frame that does not fit
      ;; is a connection error and is not debited. A frame that fits is
      ;; debited even when the stream itself is already finished.
      (when (> raw-len (http2-connection-window-size conn))
        (return-from handle-data-frame
          (connection-protocol-error conn +flow-control-error+)))
      (when (or (stream-half-closed-remote-p stream)
                (stream-closed-p stream))
        (decf (http2-connection-window-size conn) raw-len)
        (replenish-connection-window conn)
        (return-from handle-data-frame
          (connection-stream-error conn stream +stream-closed+)))
      (when (> raw-len (http2-stream-recv-window-size stream))
        (return-from handle-data-frame
          (connection-protocol-error conn +flow-control-error+)))
      ;; Padding counts against both windows and is credited back too.
      (decf (http2-connection-window-size conn) raw-len)
      (decf (http2-stream-recv-window-size stream) raw-len)
      (replenish-connection-window conn)
      (let* ((buf (http2-stream-body-buffer stream))
             (old-len (length buf))
             (new-len (+ old-len (length data))))
        (adjust-array buf new-len :fill-pointer new-len)
        (replace buf data :start1 old-len))
      (incf (http2-stream-bytes-received stream) (length data))
      (let ((declared (http2-stream-content-length stream))
            (got (http2-stream-bytes-received stream)))
        (when (and (integerp declared)
                   (or (> got declared)
                       (and end-stream (/= got declared))))
          (connection-stream-error conn stream +protocol-error+)
          (return-from handle-data-frame nil)))
      (when end-stream
        (stream-transition stream :recv-end-stream))
      (replenish-stream-window conn stream)
      (when (http2-connection-on-data conn)
        (funcall (http2-connection-on-data conn)
                 stream data end-stream))
      (connection-drop-closed-stream conn stream))))

(defun handle-ping-frame (conn frame)
  "Handle received PING frame."
  (unless (= (length (frame-payload frame)) 8)
    (return-from handle-ping-frame
      (connection-protocol-error conn +frame-size-error+)))
  (unless (zerop (frame-stream-id frame))
    (return-from handle-ping-frame
      (connection-protocol-error conn +protocol-error+)))
  (unless (logbitp 0 (frame-flags frame))  ; Not ACK
    (connection-send-frame conn
      (make-ping-ack-frame (frame-payload frame)))))

(defun handle-window-update-frame (conn frame)
  "Handle received WINDOW_UPDATE frame."
  (unless (= (length (frame-payload frame)) 4)
    (return-from handle-window-update-frame
      (connection-protocol-error conn +frame-size-error+)))
  (let ((increment (parse-window-update-payload (frame-payload frame))))
    (when (zerop increment)
      (return-from handle-window-update-frame
        (connection-protocol-error conn +protocol-error+)))
    (if (zerop (frame-stream-id frame))
        (let ((new (+ (http2-connection-remote-window-size conn) increment)))
          (when (> new +max-window-size+)
            (return-from handle-window-update-frame
              (connection-protocol-error conn +flow-control-error+)))
          (setf (http2-connection-remote-window-size conn) new)
          (connection-flush-pending-sends conn))
        (let ((stream (connection-find-stream conn (frame-stream-id frame))))
          ;; Idle (missing or never opened) is a connection error.
          ;; half-closed (remote) and closed are not.
          (cond
            ((or (null stream)
                 (= (http2-stream-state stream) +state-idle+))
             (connection-protocol-error conn +protocol-error+))
            (t
             (let ((new (+ (http2-stream-window-size stream) increment)))
               (when (> new +max-window-size+)
                 (return-from handle-window-update-frame
                   (connection-protocol-error conn +flow-control-error+)))
               (setf (http2-stream-window-size stream) new)
               (connection-flush-pending-sends conn stream))))))))

(defun handle-rst-stream-frame (conn frame)
  "Handle received RST_STREAM frame."
  (unless (= (length (frame-payload frame)) 4)
    (return-from handle-rst-stream-frame
      (connection-protocol-error conn +frame-size-error+)))
  (when (zerop (frame-stream-id frame))
    (return-from handle-rst-stream-frame
      (connection-protocol-error conn +protocol-error+)))
  (let ((stream (connection-find-stream conn (frame-stream-id frame))))
    (cond
      ((or (null stream)
           (= (http2-stream-state stream) +state-idle+))
       (connection-protocol-error conn +protocol-error+))
      ((stream-closed-p stream)
       nil)
      (t
       (stream-transition stream :recv-rst)
       (connection-drop-closed-stream conn stream)))))

(defun handle-goaway-frame (conn frame)
  "Handle received GOAWAY frame."
  (unless (zerop (frame-stream-id frame))
    (return-from handle-goaway-frame
      (connection-protocol-error conn +protocol-error+)))
  (when (< (length (frame-payload frame)) 8)
    (return-from handle-goaway-frame
      (connection-protocol-error conn +frame-size-error+)))
  (setf (http2-connection-goaway-received conn) t)
  (multiple-value-bind (last-stream-id error-code debug-data)
      (parse-goaway-payload (frame-payload frame))
    (when (http2-connection-on-goaway conn)
      (funcall (http2-connection-on-goaway conn)
               last-stream-id error-code debug-data))))

(defun handle-priority-frame (conn frame)
  "Handle received PRIORITY frame (deprecated in RFC 9113, ignore)."
  (declare (ignore conn frame))
  nil)

(defun process-frame (conn frame)
  "Process a received frame by dispatching to the appropriate handler."
  (when (http2-connection-goaway-sent conn)
    (return-from process-frame nil))
  (handler-case
      (progn
        (when (http2-connection-awaiting-first-settings conn)
          (unless (and (= (frame-type frame) +frame-settings+)
                       (not (logbitp 0 (frame-flags frame))))
            (return-from process-frame
              (connection-protocol-error conn +protocol-error+)))
          (setf (http2-connection-awaiting-first-settings conn) nil))
        (let ((awaiting (http2-connection-awaiting-continuation-stream-id conn)))
          (when awaiting
            (unless (and (= (frame-type frame) +frame-continuation+)
                         (= (frame-stream-id frame) awaiting))
              (return-from process-frame
                (connection-protocol-error conn +protocol-error+)))))
        (case (frame-type frame)
          (#.+frame-data+ (handle-data-frame conn frame))
          (#.+frame-headers+ (handle-headers-frame conn frame))
          (#.+frame-priority+ (handle-priority-frame conn frame))
          (#.+frame-rst-stream+ (handle-rst-stream-frame conn frame))
          (#.+frame-settings+ (handle-settings-frame conn frame))
          ;; Client PUSH_PROMISE is a connection error (RFC 9113 §8.4).
          (#.+frame-push-promise+
           (connection-protocol-error conn +protocol-error+))
          (#.+frame-ping+ (handle-ping-frame conn frame))
          (#.+frame-goaway+ (handle-goaway-frame conn frame))
          (#.+frame-window-update+ (handle-window-update-frame conn frame))
          (#.+frame-continuation+ (handle-continuation-frame conn frame))
          (t (vom:warn "Unknown frame type: ~A" (frame-type frame)))))
    (hpack-compression-error (e)
      (vom:error "HPACK compression error: ~A" e)
      (connection-protocol-error conn +compression-error+
                                 (trivial-utf-8:string-to-utf-8-bytes
                                  (princ-to-string e))))
    (stream-state-error (e)
      (vom:error "Stream state error: ~A" e)
      (if (stream-state-error-connection-error-p e)
          (connection-protocol-error conn (stream-state-error-code e))
          (connection-stream-error conn
                                   (stream-state-error-stream e)
                                   (stream-state-error-code e))))
    (error (e)
      (vom:error "Error processing frame: ~A" e)
      (when (http2-connection-on-error conn)
        (funcall (http2-connection-on-error conn)
                 +internal-error+
                 (trivial-utf-8:string-to-utf-8-bytes (princ-to-string e)))))))

(defun connection-process-frame (conn frame)
  "Public wrapper used by tests and higher layers."
  (process-frame conn frame))

(defun parse-connection-data (conn data start end)
  "Parse incoming data on HTTP/2 connection."
  (when (http2-connection-goaway-sent conn)
    (return-from parse-connection-data))
  (let ((buf (http2-connection-buffer conn)))
    ;; Append new data to buffer
    (let ((old-len (length buf))
          (new-len (- end start)))
      (adjust-array buf (+ old-len new-len)
                    :fill-pointer (+ old-len new-len))
      (replace buf data :start1 old-len :start2 start :end2 end))

    ;; Check for connection preface if not yet received
    (unless (http2-connection-preface-received conn)
      (when (>= (length buf) +connection-preface-length+)
        (if (equalp (subseq buf 0 +connection-preface-length+) +connection-preface+)
            (progn
              (setf (http2-connection-preface-received conn) t
                    (http2-connection-awaiting-first-settings conn) t)
              ;; Remove preface from buffer
              (let ((remaining (- (length buf) +connection-preface-length+)))
                (replace buf buf :start2 +connection-preface-length+)
                (setf (fill-pointer buf) remaining)))
            (progn
              ;; Invalid preface - send GOAWAY, then close once it is written
              (connection-send-goaway conn +protocol-error+)
              (connection-close conn)
              (return-from parse-connection-data)))))

    ;; Parse frames
    (loop
      (multiple-value-bind (frame consumed)
          (parse-frame buf
                       :max-frame-size
                       (http2-connection-local-max-frame-size conn))
        (cond
          ((eq consumed :frame-size-error)
           (connection-protocol-error conn +frame-size-error+)
           (return))
          ((null frame)
           (return))
          (t
           (process-frame conn frame)
           (when (http2-connection-goaway-sent conn)
             (setf (fill-pointer buf) 0)
             (return))
           (let ((remaining (- (length buf) consumed)))
             (replace buf buf :start2 consumed)
             (setf (fill-pointer buf) remaining))))))))

(defun setup-http2-parser (socket &key on-stream on-headers on-data on-goaway on-error on-close)
  "Set up HTTP/2 handling on socket.
   Returns the connection object.

   Callbacks:
   - on-stream: (lambda (stream)) - called when new stream created
   - on-headers: (lambda (stream headers end-stream)) - called when headers received
   - on-data: (lambda (stream data end-stream)) - called when data received
   - on-goaway: (lambda (last-stream-id error-code debug-data)) - called on GOAWAY
   - on-error: (lambda (error-code debug-data)) - called on protocol errors
   - on-close: (lambda (conn)) - called when a connection error closes the socket"
  (let ((conn (make-http2-connection
               :socket socket
               :on-stream on-stream
               :on-headers on-headers
               :on-data on-data
               :on-goaway on-goaway
               :on-error on-error
               :on-close on-close)))
    ;; Send server SETTINGS
    (connection-send-frame conn
      (make-settings-frame (http2-connection-local-settings conn)))
    (setf (http2-connection-settings-sent conn) t)

    ;; Install parser as socket read callback
    (setf (socket-data socket)
          (lambda (data &key (start 0) (end (length data)))
            (parse-connection-data conn data start end)))
    conn))
