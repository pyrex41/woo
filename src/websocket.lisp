(in-package :cl-user)
(defpackage woo.websocket
  (:use :cl)
  (:import-from :woo.ev.socket
                :socket
                :socket-data
                :socket-read-cb
                :socket-fd
                :socket-open-p
                :write-socket-data
                :write-socket-byte
                :with-async-writing
                :close-socket)
  (:import-from :woo.response
                :write-socket-string
                :write-socket-crlf)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-bytes-to-string)
  (:export :websocket-p
           :compute-accept-key
           :setup-websocket
           :send-text-frame
           :send-binary-frame
           :send-ping
           :send-pong
           :send-close
           :write-websocket-upgrade-response
           :+opcode-continuation+
           :+opcode-text+
           :+opcode-binary+
           :+opcode-close+
           :+opcode-ping+
           :+opcode-pong+
           :websocket-protocol-error
           :+max-ws-payload+))
(in-package :woo.websocket)

;; WebSocket opcodes (RFC 6455 Section 5.2)
(defconstant +opcode-continuation+ #x0)
(defconstant +opcode-text+ #x1)
(defconstant +opcode-binary+ #x2)
(defconstant +opcode-close+ #x8)
(defconstant +opcode-ping+ #x9)
(defconstant +opcode-pong+ #xA)

;; RFC 6455 magic GUID
(defvar *websocket-guid* "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

;; Cap 64-bit payload length so opcode 127 cannot allocate unbounded memory.
(defconstant +max-ws-payload+ (* 16 1024 1024))

(define-condition websocket-protocol-error (error)
  ((reason :initarg :reason :reader websocket-protocol-error-reason))
  (:report (lambda (c s)
             (format s "WebSocket protocol error: ~A"
                     (websocket-protocol-error-reason c)))))

(defun websocket-p (env)
  "Check if request is a WebSocket upgrade request."
  (and (eq (getf env :request-method) :GET)
       (let ((headers (getf env :headers)))
         (and (string-equal (gethash "upgrade" headers) "websocket")
              (search "upgrade" (string-downcase (or (gethash "connection" headers) "")))
              (gethash "sec-websocket-key" headers)
              (string= (gethash "sec-websocket-version" headers) "13")))))

(defun compute-accept-key (client-key)
  "Compute Sec-WebSocket-Accept from client's Sec-WebSocket-Key."
  (let* ((concat (concatenate 'string client-key *websocket-guid*))
         (sha1-bytes (ironclad:digest-sequence :sha1
                       (string-to-utf-8-bytes concat))))
    (cl-base64:usb8-array-to-base64-string sha1-bytes)))

(defstruct ws-state
  "WebSocket connection state."
  (buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  (fragment-opcode nil)
  (fragment-buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  ;; Set once a frame has failed the connection. Further parses must not
  ;; succeed or re-enter the error callback; the offending bytes stay buffered.
  (failed nil :type boolean)
  socket
  on-message    ; (lambda (opcode payload))
  on-ping       ; (lambda (payload))
  on-pong       ; (lambda (payload))
  on-close      ; (lambda (code reason))
  on-error)     ; (lambda (error))

(defun make-frame (opcode payload &key (fin t) (mask nil))
  "Construct a WebSocket frame as byte vector.
   Servers MUST NOT mask frames sent to clients (RFC 6455 Section 5.1)."
  (let* ((payload-len (length payload))
         (extended-len (cond ((< payload-len 126) 0)
                             ((< payload-len 65536) 2)
                             (t 8)))
         (frame-len (+ 2 extended-len (if mask 4 0) payload-len))
         (frame (make-array frame-len :element-type '(unsigned-byte 8)))
         (idx 0))
    ;; First byte: FIN + opcode
    (setf (aref frame idx) (logior (if fin #x80 0) (logand opcode #x0F)))
    (incf idx)
    ;; Second byte: MASK + payload length
    (setf (aref frame idx)
          (logior (if mask #x80 0)
                  (cond ((< payload-len 126) payload-len)
                        ((< payload-len 65536) 126)
                        (t 127))))
    (incf idx)
    ;; Extended payload length (network byte order)
    (case extended-len
      (2 (setf (aref frame idx) (ldb (byte 8 8) payload-len)
               (aref frame (1+ idx)) (ldb (byte 8 0) payload-len))
         (incf idx 2))
      (8 (loop for i from 7 downto 0
               do (setf (aref frame idx) (ldb (byte 8 (* i 8)) payload-len))
                  (incf idx))))
    ;; Masking key (servers don't mask, but included for completeness)
    (when mask
      (let ((mask-key (make-array 4 :element-type '(unsigned-byte 8))))
        (dotimes (i 4)
          (setf (aref mask-key i) (random 256)
                (aref frame idx) (aref mask-key i))
          (incf idx))
        ;; Apply mask to payload
        (dotimes (i payload-len)
          (setf (aref frame idx) (logxor (aref payload i) (aref mask-key (mod i 4))))
          (incf idx))))
    ;; Unmasked payload
    (unless mask
      (replace frame payload :start1 idx))
    frame))

(defun control-opcode-p (opcode)
  (or (= opcode +opcode-close+)
      (= opcode +opcode-ping+)
      (= opcode +opcode-pong+)))

(defun known-opcode-p (opcode)
  (or (= opcode +opcode-continuation+)
      (= opcode +opcode-text+)
      (= opcode +opcode-binary+)
      (control-opcode-p opcode)))

(defun data-opcode-p (opcode)
  (or (= opcode +opcode-text+)
      (= opcode +opcode-binary+)))

(defun close-ws (state)
  (let ((socket (ws-state-socket state)))
    (when (and socket (socket-open-p socket))
      (close-socket socket))))

(defun ws-fail (state reason)
  "Mark the connection failed and close its socket.
   Returns :ERROR. Does not consume buffered bytes: only a T return from
   PARSE-FRAME means a frame was consumed, and :ERROR must not be retried."
  (unless (ws-state-failed state)
    (setf (ws-state-failed state) t)
    (unwind-protect
         (when (ws-state-on-error state)
           (funcall (ws-state-on-error state)
                    (make-condition 'websocket-protocol-error :reason reason)))
      (close-ws state)))
  :error)

(defun append-fragment (state payload)
  "Append PAYLOAD to the open fragment. Fail the connection instead of
   growing past +MAX-WS-PAYLOAD+. Returns T on success, NIL after WS-FAIL."
  (let* ((frag (ws-state-fragment-buffer state))
         (old (length frag))
         (new (+ old (length payload))))
    (when (> new +max-ws-payload+)
      (ws-fail state "fragment exceeds maximum payload")
      (return-from append-fragment nil))
    (let ((grown (adjust-array frag new :fill-pointer new)))
      (setf (ws-state-fragment-buffer state) grown)
      (replace grown payload :start1 old))
    t))

(defun parse-frame (state)
  "Parse WebSocket frame from state buffer.
   Returns T if a complete frame was consumed, NIL if more data is needed,
   or :ERROR on an RFC 6455 protocol violation. :ERROR does not consume
   bytes and must not be treated as success: the same buffer fails again.
   A failed connection returns :ERROR without parsing further."
  (when (ws-state-failed state)
    (return-from parse-frame :error))

  (let* ((buf (ws-state-buffer state))
         (buf-len (length buf)))

    (when (< buf-len 2)
      (return-from parse-frame nil))

    (let* ((byte0 (aref buf 0))
           (byte1 (aref buf 1))
           (fin (logbitp 7 byte0))
           (rsv (logand byte0 #x70))
           (opcode (logand byte0 #x0F))
           (masked (logbitp 7 byte1))
           (len7 (logand byte1 #x7F))
           (header-len 2)
           (payload-len len7))

      (unless (zerop rsv)
        (return-from parse-frame (ws-fail state "RSV bits must be 0")))

      (unless masked
        (return-from parse-frame (ws-fail state "client frames must be masked")))

      ;; Opcodes 0x3-0x7 and 0xB-0xF are reserved (RFC 6455 5.2). No extension
      ;; is negotiated, so an unknown opcode fails the connection.
      (unless (known-opcode-p opcode)
        (return-from parse-frame (ws-fail state "reserved opcode")))

      (when (and (control-opcode-p opcode) (not fin))
        (return-from parse-frame (ws-fail state "control frames must not be fragmented")))

      (when (and (control-opcode-p opcode) (> len7 125))
        (return-from parse-frame (ws-fail state "control frame payload exceeds 125")))

      (cond
        ((= len7 126)
         (when (< buf-len 4)
           (return-from parse-frame nil))
         (setf payload-len (+ (ash (aref buf 2) 8) (aref buf 3))
               header-len 4))
        ((= len7 127)
         (when (< buf-len 10)
           (return-from parse-frame nil))
         (when (logbitp 7 (aref buf 2))
           (return-from parse-frame (ws-fail state "invalid 64-bit payload length")))
         (let ((len 0))
           (loop for i from 2 to 9
                 for shift from 56 downto 0 by 8
                 do (setf len (logior len (ash (aref buf i) shift))))
           (when (> len +max-ws-payload+)
             (return-from parse-frame (ws-fail state "payload too large")))
           (setf payload-len len
                 header-len 10))))

      (when (> payload-len +max-ws-payload+)
        (return-from parse-frame (ws-fail state "payload too large")))

      (incf header-len 4)

      (let ((frame-len (+ header-len payload-len)))
        (when (< buf-len frame-len)
          (return-from parse-frame nil))

        (let ((payload (make-array payload-len :element-type '(unsigned-byte 8)))
              (mask-start (- header-len 4)))
          (dotimes (i payload-len)
            (setf (aref payload i)
                  (logxor (aref buf (+ header-len i))
                          (aref buf (+ mask-start (mod i 4))))))

          (cond
            ((= opcode +opcode-ping+)
             (when (ws-state-on-ping state)
               (funcall (ws-state-on-ping state) payload)))
            ((= opcode +opcode-pong+)
             (when (ws-state-on-pong state)
               (funcall (ws-state-on-pong state) payload)))
            ((= opcode +opcode-close+)
             ;; A body must be empty or start with a 2-octet status code.
             ;; One octet is illegal (RFC 6455 5.5.1) and fails the connection.
             (when (= payload-len 1)
               (return-from parse-frame
                 (ws-fail state "close body must be empty or at least 2 octets")))
             (let ((code (if (>= payload-len 2)
                             (+ (ash (aref payload 0) 8) (aref payload 1))
                             1000))
                   (reason (if (> payload-len 2)
                               (utf-8-bytes-to-string payload :start 2)
                               "")))
               (when (ws-state-on-close state)
                 (funcall (ws-state-on-close state) code reason))))
            ((= opcode +opcode-continuation+)
             (unless (ws-state-fragment-opcode state)
               (return-from parse-frame
                 (ws-fail state "continuation with no message in progress")))
             (unless (append-fragment state payload)
               (return-from parse-frame :error))
             (when fin
               (when (ws-state-on-message state)
                 (funcall (ws-state-on-message state)
                          (ws-state-fragment-opcode state)
                          (ws-state-fragment-buffer state)))
               (setf (fill-pointer (ws-state-fragment-buffer state)) 0
                     (ws-state-fragment-opcode state) nil)))
            ((data-opcode-p opcode)
             (when (ws-state-fragment-opcode state)
               (return-from parse-frame
                 (ws-fail state "data frame while a message is fragmented")))
             (if fin
                 (when (ws-state-on-message state)
                   (funcall (ws-state-on-message state) opcode payload))
                 (progn
                   (setf (fill-pointer (ws-state-fragment-buffer state)) 0)
                   (setf (ws-state-fragment-opcode state) opcode)
                   (unless (append-fragment state payload)
                     (return-from parse-frame :error)))))
            (t
             (return-from parse-frame (ws-fail state "reserved opcode"))))

          (let ((remaining (- buf-len frame-len)))
            (if (zerop remaining)
                (setf (fill-pointer buf) 0)
                (progn
                  (replace buf buf :start2 frame-len)
                  (setf (fill-pointer buf) remaining))))

          t)))))

(defun send-frame (socket opcode payload)
  "Send a WebSocket frame over socket."
  (let ((frame (make-frame opcode payload)))
    (with-async-writing (socket)
      (write-socket-data socket frame))))

(defun send-text-frame (socket text)
  "Send a text frame."
  (send-frame socket +opcode-text+
              (string-to-utf-8-bytes text)))

(defun send-binary-frame (socket data)
  "Send a binary frame."
  (send-frame socket +opcode-binary+ data))

(defun send-ping (socket &optional (payload #()))
  "Send a ping frame."
  (send-frame socket +opcode-ping+ payload))

(defun send-pong (socket payload)
  "Send a pong frame."
  (send-frame socket +opcode-pong+ payload))

(defun send-close (socket &optional (code 1000) (reason ""))
  "Send a close frame."
  (let* ((reason-bytes (string-to-utf-8-bytes reason))
         (payload (make-array (+ 2 (length reason-bytes))
                              :element-type '(unsigned-byte 8))))
    (setf (aref payload 0) (ldb (byte 8 8) code)
          (aref payload 1) (ldb (byte 8 0) code))
    (replace payload reason-bytes :start1 2)
    (send-frame socket +opcode-close+ payload)))

(defun setup-websocket (socket &key on-message on-ping on-pong on-close on-error)
  "Set up WebSocket handling on an upgraded socket.
   Returns the ws-state object.

   Callbacks:
   - on-message: (lambda (opcode payload)) - called for text/binary messages
   - on-ping: (lambda (payload)) - called for ping frames (default: auto-pong)
   - on-pong: (lambda (payload)) - called for pong frames
   - on-close: (lambda (code reason)) - called for close frames (default: echo close)
   - on-error: (lambda (error)) - called on parse errors"
  (let ((state (make-ws-state
                :socket socket
                :on-message on-message
                :on-ping (or on-ping
                             (lambda (payload)
                               (send-pong socket payload)))
                :on-pong on-pong
                :on-close (or on-close
                              (lambda (code reason)
                                (declare (ignore reason))
                                (send-close socket code)
                                (close-socket socket)))
                :on-error on-error)))
    ;; Replace socket's data with our state and install frame parser
    (setf (socket-data socket)
          (lambda (data &key (start 0) (end (length data)))
            (handler-case
                (cond
                  ;; Already failed: do not parse again (the bad frame is
                  ;; still buffered, and :ERROR is truthy).
                  ((ws-state-failed state)
                   (close-ws state))
                  (t
                   (let* ((buf (ws-state-buffer state))
                          (new-len (- end start))
                          (old-len (length buf))
                          (grown (adjust-array buf (+ old-len new-len)
                                               :fill-pointer (+ old-len new-len))))
                     (setf (ws-state-buffer state) grown)
                     (replace grown data :start1 old-len :start2 start :end2 end))
                     ;; Only T means a frame was consumed. NIL waits for more
                     ;; bytes. :ERROR leaves the buffer unchanged; retrying it
                     ;; would spin the worker.
                     (loop while (eq (parse-frame state) t))))
              (error (e)
                (when (ws-state-on-error state)
                  (funcall (ws-state-on-error state) e))))))
    state))

(defun write-websocket-upgrade-response (socket accept-key &optional extra-headers)
  "Write a 101 Switching Protocols response for WebSocket upgrade.
   EXTRA-HEADERS is a plist of additional headers to include."
  (with-async-writing (socket)
    (write-socket-data socket #.(string-to-utf-8-bytes "HTTP/1.1 101 Switching Protocols\r\n"))
    (write-socket-data socket #.(string-to-utf-8-bytes "Upgrade: websocket\r\n"))
    (write-socket-data socket #.(string-to-utf-8-bytes "Connection: Upgrade\r\n"))
    (write-socket-data socket #.(string-to-utf-8-bytes "Sec-WebSocket-Accept: "))
    (write-socket-string socket accept-key)
    (write-socket-crlf socket)
    ;; Extra headers (e.g., Sec-WebSocket-Protocol)
    (loop for (k v) on extra-headers by #'cddr
          when v
          do (write-socket-string socket (format nil "~:(~A~): ~A" k v))
             (write-socket-crlf socket))
    (write-socket-crlf socket)))
