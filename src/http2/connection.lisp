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
           :connection-process-frame
           :http2-connection-remote-window-size
           :http2-connection-remote-max-frame-size
           :http2-connection-local-max-frame-size
           :http2-connection-awaiting-continuation-stream-id
           :setup-http2-parser))
(in-package :woo.http2.connection)

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
  ;; Connection-level flow control
  (window-size +default-initial-window-size+ :type integer)
  (remote-window-size +default-initial-window-size+ :type integer)
  ;; Settings
  (local-settings `((,+settings-max-concurrent-streams+ . 100)
                    (,+settings-initial-window-size+ . ,+default-initial-window-size+)
                    (,+settings-max-frame-size+ . ,+default-max-frame-size+)
                    (,+settings-header-table-size+ . ,+default-header-table-size+)))
  (remote-settings nil)
  (remote-max-frame-size +default-max-frame-size+ :type integer)
  (remote-initial-window-size +default-initial-window-size+ :type integer)
  (local-max-frame-size +default-max-frame-size+ :type integer)
  (local-max-concurrent-streams 100 :type integer)
  (awaiting-continuation-stream-id nil)
  ;; Callbacks
  on-stream       ; (lambda (stream)) - called for new stream
  on-headers      ; (lambda (stream headers end-stream))
  on-data         ; (lambda (stream data end-stream))
  on-goaway       ; (lambda (last-stream-id error-code debug-data))
  on-error)       ; (lambda (error-code debug-data))

(defun connection-get-stream (conn stream-id &key (create nil))
  "Get stream by ID, optionally creating it if it doesn't exist."
  (let ((streams (http2-connection-streams conn)))
    (or (gethash stream-id streams)
        (when create
          (let ((stream (make-http2-stream
                         :id stream-id
                         :window-size (http2-connection-remote-initial-window-size conn)
                         :recv-window-size +default-initial-window-size+))))
            (setf (gethash stream-id streams) stream)
            (when (http2-connection-on-stream conn)
              (funcall (http2-connection-on-stream conn) stream))
            stream)))))

(defun connection-send-frame (conn frame)
  "Send a frame on the connection."
  (let ((socket (http2-connection-socket conn)))
    (when (and socket (socket-open-p socket))
      (with-async-writing (socket)
        (write-socket-data socket (serialize-frame frame))))))

(defun connection-send-goaway (conn error-code &optional debug-data)
  "Send GOAWAY frame and mark connection for shutdown."
  (unless (http2-connection-goaway-sent conn)
    (setf (http2-connection-goaway-sent conn) t)
    (connection-send-frame conn
      (make-goaway-frame (http2-connection-last-stream-id conn)
                         error-code
                         debug-data))))

;;; Frame handlers

(defun connection-protocol-error (conn error-code &optional debug-data)
  "Record a connection error, send GOAWAY, invoke on-error."
  (connection-send-goaway conn error-code debug-data)
  (when (http2-connection-on-error conn)
    (funcall (http2-connection-on-error conn) error-code debug-data))
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
         (connection-send-frame conn (make-settings-ack-frame)))))))

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
     (let ((existing (gethash stream-id (http2-connection-streams conn))))
       (cond
         ((null existing)
          (when (<= stream-id (http2-connection-last-stream-id conn))
            (connection-protocol-error conn +protocol-error+)
            (return-from validate-new-stream-id nil))
          (when (>= (connection-open-stream-count conn)
                    (http2-connection-local-max-concurrent-streams conn))
            (connection-protocol-error conn +protocol-error+)
            (return-from validate-new-stream-id nil))
          t)
         ((or (stream-closed-p existing)
              (stream-half-closed-remote-p existing))
          (connection-protocol-error conn +protocol-error+)
          nil)
         (t t))))))

(defun finish-header-block (conn stream stream-id header-block end-stream)
  (let ((headers (hpack-decode-headers
                  (http2-connection-decoder-context conn)
                  header-block)))
    (setf (http2-stream-headers stream) headers
          (http2-stream-header-buffer stream) nil
          (http2-stream-awaiting-continuation stream) nil
          (http2-stream-pending-end-stream stream) nil
          (http2-connection-awaiting-continuation-stream-id conn) nil)
    (stream-transition stream :recv-headers)
    (when end-stream
      (stream-transition stream :recv-end-stream))
    (when (> stream-id (http2-connection-last-stream-id conn))
      (setf (http2-connection-last-stream-id conn) stream-id))
    (let ((cl (cdr (assoc "content-length" headers :test #'string=))))
      (when cl
        (setf (http2-stream-content-length stream)
              (parse-integer cl))))
    (when (http2-connection-on-headers conn)
      (funcall (http2-connection-on-headers conn)
               stream headers end-stream))))

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
        (let ((header-block (subseq payload header-start header-end))
              (stream (connection-get-stream conn stream-id :create t)))
          (if end-headers
              (finish-header-block conn stream stream-id header-block end-stream)
              (progn
                (setf (http2-stream-header-buffer stream) header-block
                      (http2-stream-awaiting-continuation stream) t
                      (http2-stream-pending-end-stream stream) end-stream
                      (http2-connection-awaiting-continuation-stream-id conn)
                      stream-id))))))))

(defun handle-continuation-frame (conn frame)
  "Handle received CONTINUATION frame."
  (let* ((stream-id (frame-stream-id frame))
         (awaiting (http2-connection-awaiting-continuation-stream-id conn))
         (stream (gethash stream-id (http2-connection-streams conn))))
    (unless (and awaiting stream
                 (= awaiting stream-id)
                 (http2-stream-awaiting-continuation stream))
      (return-from handle-continuation-frame
        (connection-protocol-error conn +protocol-error+)))
    (let* ((payload (frame-payload frame))
           (end-headers (logbitp 2 (frame-flags frame)))
           (old-buffer (http2-stream-header-buffer stream))
           (new-buffer (concatenate '(vector (unsigned-byte 8))
                                    old-buffer payload)))
      (if end-headers
          (finish-header-block conn stream stream-id new-buffer
                               (http2-stream-pending-end-stream stream))
          (setf (http2-stream-header-buffer stream) new-buffer)))))

(defun handle-data-frame (conn frame)
  "Handle received DATA frame."
  (let* ((stream-id (frame-stream-id frame))
         (stream (gethash stream-id (http2-connection-streams conn)))
         (flags (frame-flags frame))
         (end-stream (logbitp 0 flags))
         (raw-len (length (frame-payload frame))))
    (when (zerop stream-id)
      (return-from handle-data-frame
        (connection-protocol-error conn +protocol-error+)))
    (unless stream
      (return-from handle-data-frame
        (connection-protocol-error conn +protocol-error+)))
    (when (or (stream-half-closed-remote-p stream)
              (stream-closed-p stream))
      (return-from handle-data-frame
        (connection-protocol-error conn +stream-closed+)))
    (multiple-value-bind (data pad-error)
        (unpadded-payload (frame-payload frame) flags)
      (when pad-error
        (return-from handle-data-frame
          (connection-protocol-error conn pad-error)))
      (when (or (> raw-len (http2-connection-window-size conn))
                (> raw-len (http2-stream-recv-window-size stream)))
        (return-from handle-data-frame
          (connection-protocol-error conn +flow-control-error+)))
      (decf (http2-connection-window-size conn) raw-len)
      (decf (http2-stream-recv-window-size stream) raw-len)
      (let* ((buf (http2-stream-body-buffer stream))
             (old-len (length buf))
             (new-len (+ old-len (length data))))
        (adjust-array buf new-len :fill-pointer new-len)
        (replace buf data :start1 old-len))
      (incf (http2-stream-bytes-received stream) (length data))
      (when end-stream
        (stream-transition stream :recv-end-stream))
      (when (http2-connection-on-data conn)
        (funcall (http2-connection-on-data conn)
                 stream data end-stream)))))

(defun handle-ping-frame (conn frame)
  "Handle received PING frame."
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
          (setf (http2-connection-remote-window-size conn) new))
        (let ((stream (gethash (frame-stream-id frame)
                               (http2-connection-streams conn))))
          (when stream
            (let ((new (+ (http2-stream-window-size stream) increment)))
              (when (> new +max-window-size+)
                (return-from handle-window-update-frame
                  (connection-protocol-error conn +flow-control-error+)))
              (setf (http2-stream-window-size stream) new)))))))

(defun handle-rst-stream-frame (conn frame)
  "Handle received RST_STREAM frame."
  (let ((stream (gethash (frame-stream-id frame)
                         (http2-connection-streams conn))))
    (when stream
      (stream-transition stream :recv-rst))))

(defun handle-goaway-frame (conn frame)
  "Handle received GOAWAY frame."
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
  (handler-case
      (progn
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
          (#.+frame-push-promise+ nil)
          (#.+frame-ping+ (handle-ping-frame conn frame))
          (#.+frame-goaway+ (handle-goaway-frame conn frame))
          (#.+frame-window-update+ (handle-window-update-frame conn frame))
          (#.+frame-continuation+ (handle-continuation-frame conn frame))
          (t (vom:warn "Unknown frame type: ~A" (frame-type frame)))))
    (hpack-compression-error (e)
      (vom:error "HPACK compression error: ~A" e)
      (connection-send-goaway conn +compression-error+
                              (trivial-utf-8:string-to-utf-8-bytes
                               (princ-to-string e)))
      (when (http2-connection-on-error conn)
        (funcall (http2-connection-on-error conn)
                 +compression-error+
                 (trivial-utf-8:string-to-utf-8-bytes (princ-to-string e)))))
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
              (setf (http2-connection-preface-received conn) t)
              ;; Remove preface from buffer
              (let ((remaining (- (length buf) +connection-preface-length+)))
                (replace buf buf :start2 +connection-preface-length+)
                (setf (fill-pointer buf) remaining)))
            (progn
              ;; Invalid preface - send GOAWAY and close
              (connection-send-goaway conn +protocol-error+)
              (close-socket (http2-connection-socket conn))
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
           (let ((remaining (- (length buf) consumed)))
             (replace buf buf :start2 consumed)
             (setf (fill-pointer buf) remaining))))))))

(defun setup-http2-parser (socket &key on-stream on-headers on-data on-goaway on-error)
  "Set up HTTP/2 handling on socket.
   Returns the connection object.

   Callbacks:
   - on-stream: (lambda (stream)) - called when new stream created
   - on-headers: (lambda (stream headers end-stream)) - called when headers received
   - on-data: (lambda (stream data end-stream)) - called when data received
   - on-goaway: (lambda (last-stream-id error-code debug-data)) - called on GOAWAY
   - on-error: (lambda (error-code debug-data)) - called on protocol errors"
  (let ((conn (make-http2-connection
               :socket socket
               :on-stream on-stream
               :on-headers on-headers
               :on-data on-data
               :on-goaway on-goaway
               :on-error on-error)))
    ;; Send server SETTINGS
    (connection-send-frame conn
      (make-settings-frame (http2-connection-local-settings conn)))
    (setf (http2-connection-settings-sent conn) t)

    ;; Install parser as socket read callback
    (setf (socket-data socket)
          (lambda (data &key (start 0) (end (length data)))
            (parse-connection-data conn data start end)))
    conn))
