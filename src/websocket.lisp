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
           :websocket-protocol-error-code
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
  ((reason :initarg :reason :reader websocket-protocol-error-reason)
   ;; Close status the connection was failed with (RFC 6455 7.4.1).
   (code :initarg :code :initform 1002 :reader websocket-protocol-error-code))
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
  ;; Offset of the first unparsed octet. Frames are consumed by advancing it;
  ;; the reader compacts the buffer once per read, not once per frame.
  (read-pos 0 :type fixnum)
  (fragment-opcode nil)
  (fragment-buffer (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
  ;; Set once a frame has failed the connection. Further parses must not
  ;; succeed or re-enter the error callback; the offending bytes stay buffered.
  (failed nil :type boolean)
  ;; Close status sent when the connection was failed.
  (close-code nil)
  ;; A close frame arrived; later frames are discarded (RFC 6455 5.5.1).
  (close-received nil :type boolean)
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

(defun valid-close-code-p (code)
  "Status codes that may appear in a close frame (RFC 6455 7.4, IANA).
   1004-1006 and 1015 are reserved or local-only; 1016-2999 are unassigned."
  (or (<= 1000 code 1003)
      (<= 1007 code 1014)
      (<= 3000 code 4999)))

(defun valid-utf-8-p (octets &key (start 0) (end (length octets)))
  "Strict UTF-8 check: no overlongs, surrogates, or code points past U+10FFFF."
  (let ((i start))
    (loop
      (when (>= i end)
        (return t))
      (let ((b (aref octets i))
            (n 0)
            (lo #x80)
            (hi #xBF))
        (cond ((< b #x80))
              ((<= #xC2 b #xDF) (setf n 1))
              ((= b #xE0) (setf n 2 lo #xA0))
              ((= b #xED) (setf n 2 hi #x9F))
              ((<= #xE1 b #xEF) (setf n 2))
              ((= b #xF0) (setf n 3 lo #x90))
              ((<= #xF1 b #xF3) (setf n 3))
              ((= b #xF4) (setf n 3 hi #x8F))
              (t (return nil)))
        (when (>= (+ i n) end)
          (unless (zerop n)
            (return nil)))
        (loop for k from 1 to n
              for c = (aref octets (+ i k))
              unless (if (= k 1) (<= lo c hi) (<= #x80 c #xBF))
                do (return-from valid-utf-8-p nil))
        (incf i (1+ n))))))

(defun close-payload (code)
  (let ((payload (make-array 2 :element-type '(unsigned-byte 8))))
    (setf (aref payload 0) (ldb (byte 8 8) code)
          (aref payload 1) (ldb (byte 8 0) code))
    payload))

(defun close-after-frame (socket code)
  "Send a close frame with CODE and close SOCKET once it is flushed.
   Closing at once would drop the frame from the write buffer. Without a
   running event loop (unit tests) there is nothing to flush; close now."
  (when (and socket (socket-open-p socket))
    (if woo.ev.event-loop:*evloop*
        (with-async-writing (socket :write-cb (lambda (s) (close-socket s)))
          (write-socket-data socket (make-frame +opcode-close+ (close-payload code))))
        (close-socket socket))))

(defun ws-fail (state reason &optional (code 1002) condition)
  "Mark the connection failed, report it, and close with status CODE.
   CONDITION, when given, is passed to on-error instead of a new
   WEBSOCKET-PROTOCOL-ERROR (an application callback's own error).
   Returns :ERROR. A failed connection is never parsed again."
  (unless (ws-state-failed state)
    (setf (ws-state-failed state) t
          (ws-state-close-code state) code)
    (unwind-protect
         (when (ws-state-on-error state)
           (funcall (ws-state-on-error state)
                    (or condition
                        (make-condition 'websocket-protocol-error
                                        :reason reason :code code))))
      (close-after-frame (ws-state-socket state) code)))
  :error)

(defun run-callback (state fn &rest args)
  "Call an application callback on an already-consumed frame. An error fails
   the connection with 1011 rather than redelivering the frame on the next
   read. Returns T, or :ERROR after WS-FAIL."
  (if (null fn)
      t
      (handler-case (progn (apply fn args) t)
        (error (e)
          (ws-fail state (princ-to-string e) 1011 e)))))

(defun compact-buffer (state)
  "Drop consumed octets from the front of the buffer."
  (let ((pos (ws-state-read-pos state)))
    (when (plusp pos)
      (let* ((buf (ws-state-buffer state))
             (remaining (- (length buf) pos)))
        (replace buf buf :start2 pos)
        (setf (fill-pointer buf) remaining
              (ws-state-read-pos state) 0)))))

(defun append-fragment (state payload)
  "Append PAYLOAD to the open fragment. Fail the connection instead of
   growing past +MAX-WS-PAYLOAD+. Returns T on success, NIL after WS-FAIL."
  (let* ((frag (ws-state-fragment-buffer state))
         (old (length frag))
         (new (+ old (length payload))))
    (when (> new +max-ws-payload+)
      (ws-fail state "fragment exceeds maximum payload" 1009)
      (return-from append-fragment nil))
    (let ((grown (adjust-array frag new :fill-pointer new)))
      (setf (ws-state-fragment-buffer state) grown)
      (replace grown payload :start1 old))
    t))

(defun parse-frame (state)
  "Parse one WebSocket frame from the state buffer at READ-POS.
   Returns T if a complete frame was consumed, NIL if more data is needed,
   or :ERROR once the connection has failed. A frame rejected by RFC 6455
   is not consumed; a frame is consumed before any callback runs, so a
   callback error fails the connection (1011) instead of redelivering it.
   A failed connection returns :ERROR without parsing further."
  (when (ws-state-failed state)
    (return-from parse-frame :error))

  (let* ((buf (ws-state-buffer state))
         (pos (ws-state-read-pos state))
         (buf-len (- (length buf) pos)))

    ;; Nothing after a close frame is processed.
    (when (ws-state-close-received state)
      (setf (fill-pointer buf) 0
            (ws-state-read-pos state) 0)
      (return-from parse-frame nil))

    (when (< buf-len 2)
      (return-from parse-frame nil))

    (let* ((byte0 (aref buf pos))
           (byte1 (aref buf (+ pos 1)))
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
         (setf payload-len (+ (ash (aref buf (+ pos 2)) 8) (aref buf (+ pos 3)))
               header-len 4))
        ((= len7 127)
         (when (< buf-len 10)
           (return-from parse-frame nil))
         (when (logbitp 7 (aref buf (+ pos 2)))
           (return-from parse-frame (ws-fail state "invalid 64-bit payload length")))
         (let ((len 0))
           (loop for i from 2 to 9
                 for shift from 56 downto 0 by 8
                 do (setf len (logior len (ash (aref buf (+ pos i)) shift))))
           (when (> len +max-ws-payload+)
             (return-from parse-frame (ws-fail state "payload too large" 1009)))
           (setf payload-len len
                 header-len 10))))

      (when (> payload-len +max-ws-payload+)
        (return-from parse-frame (ws-fail state "payload too large" 1009)))

      (incf header-len 4)

      (let ((frame-len (+ header-len payload-len)))
        (when (< buf-len frame-len)
          (return-from parse-frame nil))

        (let ((payload (make-array payload-len :element-type '(unsigned-byte 8)))
              (mask-start (+ pos (- header-len 4)))
              (data-start (+ pos header-len)))
          (dotimes (i payload-len)
            (setf (aref payload i)
                  (logxor (aref buf (+ data-start i))
                          (aref buf (+ mask-start (mod i 4))))))

          (flet ((consume ()
                   ;; Advance past this frame; reset when the buffer drains.
                   (let ((next (+ pos frame-len)))
                     (if (= next (length buf))
                         (setf (fill-pointer buf) 0
                               (ws-state-read-pos state) 0)
                         (setf (ws-state-read-pos state) next)))))
            (cond
              ((= opcode +opcode-ping+)
               (consume)
               (run-callback state (ws-state-on-ping state) payload))
              ((= opcode +opcode-pong+)
               (consume)
               (run-callback state (ws-state-on-pong state) payload))
              ((= opcode +opcode-close+)
               ;; A body must be empty or start with a 2-octet status code.
               ;; One octet is illegal (RFC 6455 5.5.1) and fails the connection.
               (when (= payload-len 1)
                 (return-from parse-frame
                   (ws-fail state "close body must be empty or at least 2 octets")))
               (let ((code (if (>= payload-len 2)
                               (+ (ash (aref payload 0) 8) (aref payload 1))
                               1000)))
                 (unless (valid-close-code-p code)
                   (return-from parse-frame (ws-fail state "invalid close code")))
                 (unless (valid-utf-8-p payload :start (min 2 payload-len))
                   (return-from parse-frame
                     (ws-fail state "close reason is not valid UTF-8" 1007)))
                 (setf (ws-state-close-received state) t)
                 (consume)
                 (run-callback state (ws-state-on-close state) code
                               (if (> payload-len 2)
                                   (utf-8-bytes-to-string payload :start 2)
                                   ""))))
              ((= opcode +opcode-continuation+)
               (unless (ws-state-fragment-opcode state)
                 (return-from parse-frame
                   (ws-fail state "continuation with no message in progress")))
               (unless (append-fragment state payload)
                 (return-from parse-frame :error))
               (if fin
                   ;; Deliver a fresh copy: the fragment buffer is reused.
                   (let* ((frag (ws-state-fragment-buffer state))
                          (message (replace (make-array (length frag)
                                                        :element-type '(unsigned-byte 8))
                                            frag))
                          (message-opcode (ws-state-fragment-opcode state)))
                     (when (and (= message-opcode +opcode-text+)
                                (not (valid-utf-8-p message)))
                       (return-from parse-frame
                         (ws-fail state "text message is not valid UTF-8" 1007)))
                     (setf (fill-pointer frag) 0
                           (ws-state-fragment-opcode state) nil)
                     (consume)
                     (run-callback state (ws-state-on-message state)
                                   message-opcode message))
                   (progn (consume) t)))
              ((data-opcode-p opcode)
               (when (ws-state-fragment-opcode state)
                 (return-from parse-frame
                   (ws-fail state "data frame while a message is fragmented")))
               (cond
                 (fin
                  (when (and (= opcode +opcode-text+)
                             (not (valid-utf-8-p payload)))
                    (return-from parse-frame
                      (ws-fail state "text message is not valid UTF-8" 1007)))
                  (consume)
                  (run-callback state (ws-state-on-message state) opcode payload))
                 (t
                  (setf (fill-pointer (ws-state-fragment-buffer state)) 0)
                  (setf (ws-state-fragment-opcode state) opcode)
                  (unless (append-fragment state payload)
                    (return-from parse-frame :error))
                  (consume)
                  t)))
              (t
               (ws-fail state "reserved opcode")))))))))

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
   - on-error: (lambda (error)) - called when the connection fails: a
     WEBSOCKET-PROTOCOL-ERROR, or the error a callback raised (closed 1011)"
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
                                ;; Received codes are validated, but never
                                ;; echo a reserved/local-only one.
                                (close-after-frame socket
                                                   (if (valid-close-code-p code)
                                                       code
                                                       1000))))
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
                   ;; bytes. :ERROR is final; retrying it would spin the worker.
                   (unwind-protect
                        (loop while (eq (parse-frame state) t))
                     (compact-buffer state))))
              (error (e)
                ;; Anything unexpected fails the connection: leaving it open
                ;; would re-parse the same bytes on the next read.
                (ws-fail state (princ-to-string e) 1011 e)))))
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
