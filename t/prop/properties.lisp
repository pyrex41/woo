(in-package :cl-user)
(defpackage woo-test.prop.properties
  (:use :cl :rove :woo-test.prop)
  (:import-from :woo.http2.hpack
                :make-hpack-context
                :hpack-encode-headers
                :hpack-decode-headers
                :hpack-compression-error)
  (:import-from :woo.http2.frames
                :make-frame
                :frame-type
                :frame-flags
                :frame-stream-id
                :frame-payload
                :parse-frame
                :serialize-frame
                :make-settings-frame
                :make-headers-frame
                :make-data-frame
                :make-window-update-frame
                :parse-settings-payload
                :parse-window-update-payload)
  (:import-from :woo.http2.connection
                :make-http2-connection
                :connection-process-frame
                :http2-connection-goaway-sent)
  (:import-from :woo.http2.constants
                :+frame-data+
                :+frame-headers+
                :+frame-settings+
                :+frame-window-update+
                :+flag-end-headers+
                :+flag-padded+
                :+flag-ack+
                :+settings-max-frame-size+
                :+settings-initial-window-size+
                :+protocol-error+
                :+flow-control-error+
                :+min-max-frame-size+
                :+max-frame-size-limit+
                :+max-window-size+)
  (:import-from :woo.websocket
                :+opcode-text+
                :+opcode-binary+
                :+opcode-close+
                :+opcode-ping+
                :+opcode-pong+
                :websocket-protocol-error
                :+max-ws-payload+))
(in-package :woo-test.prop.properties)

;;; Generators

(defun gen-ascii-string (rng size)
  (let* ((n (rng-int rng 0 (min 24 size)))
         (s (make-string n)))
    (dotimes (i n)
      (setf (char s i) (code-char (rng-int rng 97 122))))
    s))

(defun gen-header (rng size)
  (cons (or (and (rng-bool rng)
                 (rng-choose rng '(":method" ":path" ":scheme" ":authority"
                                   "content-type" "user-agent" "accept" "host")))
            (gen-ascii-string rng (max 1 size)))
        (gen-ascii-string rng size)))

(defun gen-header-list (rng size)
  (loop repeat (rng-int rng 0 (min 8 size))
        collect (gen-header rng size)))

(defun gen-hpack-integer (rng size)
  (rng-int rng 0 (min (expt 2 (min 20 size)) #xFFFFFF)))

(defun gen-frame-bytes (rng size)
  (let* ((plen (rng-int rng 0 (min 64 size)))
         (type (rng-choose rng '(0 1 4 6 7 8)))
         (flags (rng-uint rng 256))
         (sid (rng-int rng 0 (min #x7FFFFFFF (* size 17))))
         (payload (make-array plen :element-type '(unsigned-byte 8)))
         (frame (make-array (+ 9 plen) :element-type '(unsigned-byte 8))))
    (dotimes (i plen) (setf (aref payload i) (rng-uint rng 256)))
    (setf (aref frame 0) (ldb (byte 8 16) plen)
          (aref frame 1) (ldb (byte 8 8) plen)
          (aref frame 2) (ldb (byte 8 0) plen)
          (aref frame 3) type
          (aref frame 4) flags
          (aref frame 5) (ldb (byte 8 24) sid)
          (aref frame 6) (ldb (byte 8 16) sid)
          (aref frame 7) (ldb (byte 8 8) sid)
          (aref frame 8) (ldb (byte 8 0) sid))
    (replace frame payload :start1 9)
    frame))

(defun gen-ws-payload (rng size)
  (let* ((n (rng-int rng 0 (min 80 size)))
         (p (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n) (setf (aref p i) (rng-uint rng 256)))
    p))

(defun write-u16 (vec idx n)
  (setf (aref vec idx) (ldb (byte 8 8) n)
        (aref vec (1+ idx) ) (ldb (byte 8 0) n)))

(defun write-u64 (vec idx n)
  (loop for i from 0 below 8
        do (setf (aref vec (+ idx i)) (ldb (byte 8 (* (- 7 i) 8)) n))))

(defun encode-ws-frame (opcode payload &key (fin t) (mask t) (mask-key #(1 2 3 4)) (rsv 0))
  (let* ((plen (length payload))
         (ext (cond ((< plen 126) 0) ((< plen 65536) 2) (t 8)))
         (mlen (if mask 4 0))
         (frame (make-array (+ 2 ext mlen plen) :element-type '(unsigned-byte 8)))
         (idx 0))
    (setf (aref frame idx) (logior (if fin #x80 0) (logand rsv #x70) (logand opcode #x0F)))
    (incf idx)
    (setf (aref frame idx)
          (logior (if mask #x80 0)
                  (cond ((< plen 126) plen)
                        ((< plen 65536) 126)
                        (t 127))))
    (incf idx)
    (case ext
      (2 (write-u16 frame idx plen) (incf idx 2))
      (8 (write-u64 frame idx plen) (incf idx 8)))
    (when mask
      (replace frame mask-key :start1 idx)
      (incf idx 4)
      (dotimes (i plen)
        (setf (aref frame (+ idx i))
              (logxor (aref payload i) (aref mask-key (mod i 4))))))
    (unless mask
      (replace frame payload :start1 idx))
    frame))

(defun gen-legal-ws-frame (rng size)
  (let ((opcode (rng-choose rng (list +opcode-text+ +opcode-binary+)))
        (payload (gen-ws-payload rng size))
        (key (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4) (setf (aref key i) (rng-uint rng 256)))
    (encode-ws-frame opcode payload :mask t :mask-key key)))

;;; Properties

(defun integer-roundtrip (value)
  (let* ((prefix (1+ (mod value 7)))
         (bytes (woo.http2.hpack::hpack-encode-integer value prefix 0))
         (vec (make-array (length bytes) :element-type '(unsigned-byte 8)
                          :initial-contents bytes)))
    (multiple-value-bind (decoded consumed)
        (woo.http2.hpack::hpack-decode-integer vec 0 prefix)
      (and (= decoded value) (= consumed (length vec))))))

(defun string-roundtrip (str)
  (flet ((once (huffman)
           (let* ((enc (woo.http2.hpack::hpack-encode-string str :huffman huffman)))
             (multiple-value-bind (decoded consumed)
                 (woo.http2.hpack::hpack-decode-string enc 0)
               (and (string= decoded str) (= consumed (length enc)))))))
    (and (once nil) (once t))))

(defun header-roundtrip (headers)
  (let* ((enc-ctx (make-hpack-context))
         (dec-ctx (make-hpack-context))
         (block (hpack-encode-headers enc-ctx headers))
         (decoded (hpack-decode-headers dec-ctx block)))
    (equal decoded headers)))

(defun huffman-bytes-roundtrip (bytes)
  (let* ((enc (woo.http2.hpack::huffman-encode-bytes bytes))
         (dec (woo.http2.hpack::huffman-decode-bytes enc 0 (length enc))))
    (bytes-equal-p bytes dec)))

(defun frame-roundtrip (bytes)
  (multiple-value-bind (frame consumed) (parse-frame bytes)
    (and frame
         (= consumed (length bytes))
         (bytes-equal-p bytes (serialize-frame frame)))))

(defun process-error-code (frame)
  (let ((code nil)
        (conn (make-http2-connection
               :on-error (lambda (c d) (declare (ignore d)) (setf code c)))))
    (connection-process-frame conn frame)
    (values code (http2-connection-goaway-sent conn))))

(defun ws-parse-result (bytes)
  (let ((err nil)
        (msgs nil))
    (let ((state (woo.websocket::make-ws-state
                  :on-error (lambda (e) (setf err e))
                  :on-message (lambda (op p) (push (cons op p) msgs))
                  :on-ping (lambda (p) (push (cons :ping p) msgs))
                  :on-pong (lambda (p) (push (cons :pong p) msgs))
                  :on-close (lambda (c r) (push (list :close c r) msgs)))))
      (let ((buf (woo.websocket::ws-state-buffer state)))
        (adjust-array buf (length bytes) :fill-pointer (length bytes))
        (replace buf bytes)
        (setf (woo.websocket::ws-state-buffer state) buf))
      (values (woo.websocket::parse-frame state) err msgs))))

(deftest prop-hpack-integer-roundtrip
  (ok (check-property "hpack-integer-roundtrip"
                      #'integer-roundtrip
                      (lambda (rng size) (gen-hpack-integer rng size))
                      :shrinker #'shrink-int)
      "HPACK integer encode/decode round-trip"))

(deftest prop-hpack-string-roundtrip
  (ok (check-property "hpack-string-roundtrip"
                      #'string-roundtrip
                      #'gen-ascii-string
                      :shrinker (lambda (s)
                                  (when (> (length s) 0)
                                    (list "" (subseq s 0 (floor (length s) 2))
                                          (subseq s 0 (1- (length s)))))))
      "HPACK string (raw + Huffman) round-trip"))

(deftest prop-hpack-headers-roundtrip
  (ok (check-property "hpack-headers-roundtrip"
                      #'header-roundtrip
                      #'gen-header-list
                      :shrinker (lambda (hs)
                                  (shrink-list hs (lambda (h)
                                                    (list (cons "" "")
                                                          (cons (car h) "")
                                                          (cons "" (cdr h)))))))
      "HPACK header list round-trip"))

(deftest prop-huffman-bytes-roundtrip
  (ok (check-property "huffman-bytes-roundtrip"
                      #'huffman-bytes-roundtrip
                      (lambda (rng size)
                        (let* ((n (rng-int rng 0 (min 32 size)))
                               (v (make-array n :element-type '(unsigned-byte 8))))
                          (dotimes (i n) (setf (aref v i) (rng-uint rng 256)))
                          v))
                      :shrinker #'shrink-vector)
      "Huffman encode/decode round-trip"))

(deftest prop-http2-frame-roundtrip
  (ok (check-property "http2-frame-bytes-roundtrip"
                      #'frame-roundtrip
                      #'gen-frame-bytes
                      :shrinker #'shrink-vector)
      "HTTP/2 serialize/parse round-trip"))

(deftest prop-illegal-stream-ids
  (ok (check-property "headers-stream-0-protocol-error"
                      (lambda (sid)
                        (declare (ignore sid))
                        (multiple-value-bind (code goaway)
                            (process-error-code
                             (make-headers-frame 0 #() :end-headers t))
                          (and goaway (eql code +protocol-error+))))
                      (lambda (rng size) (declare (ignore rng size)) 0))
      "HEADERS on stream 0 is PROTOCOL_ERROR")
  (ok (check-property "headers-even-client-id-protocol-error"
                      (lambda (sid)
                        (multiple-value-bind (code goaway)
                            (process-error-code
                             (make-headers-frame sid #() :end-headers t))
                          (and goaway (eql code +protocol-error+))))
                      (lambda (rng size)
                        (let ((n (rng-int rng 1 (max 2 size))))
                          (* 2 n)))
                      :shrinker (lambda (n) (when (> n 2) (list 2))))
      "even client stream IDs are PROTOCOL_ERROR")
  (ok (check-property "data-stream-0-protocol-error"
                      (lambda (_)
                        (declare (ignore _))
                        (multiple-value-bind (code goaway)
                            (process-error-code
                             (make-data-frame 0 (make-array 1 :element-type '(unsigned-byte 8)
                                                            :initial-element 0)))
                          (and goaway (eql code +protocol-error+))))
                      (lambda (rng size) (declare (ignore rng size)) 0))
      "DATA on stream 0 is PROTOCOL_ERROR"))

(deftest prop-window-update-zero
  (ok (check-property "window-update-zero-protocol-error"
                      (lambda (_)
                        (declare (ignore _))
                        (multiple-value-bind (code goaway)
                            (process-error-code (make-window-update-frame 0 0))
                          (and goaway (eql code +protocol-error+))))
                      (lambda (rng size) (declare (ignore rng size)) 0))
      "WINDOW_UPDATE increment 0 is PROTOCOL_ERROR"))

(deftest prop-settings-out-of-range
  (ok (check-property "settings-max-frame-size-too-small"
                      (lambda (v)
                        (multiple-value-bind (code goaway)
                            (process-error-code
                             (make-settings-frame
                              (list (cons +settings-max-frame-size+ v))))
                          (and goaway (eql code +protocol-error+))))
                      (lambda (rng size)
                        (declare (ignore size))
                        (rng-int rng 0 (1- +min-max-frame-size+)))
                      :shrinker #'shrink-int)
      "SETTINGS MAX_FRAME_SIZE < 16384 is PROTOCOL_ERROR")
  (ok (check-property "settings-max-frame-size-too-large"
                      (lambda (v)
                        (multiple-value-bind (code goaway)
                            (process-error-code
                             (make-settings-frame
                              (list (cons +settings-max-frame-size+ v))))
                          (and goaway (eql code +protocol-error+))))
                      (lambda (rng size)
                        (declare (ignore size))
                        (rng-int rng (1+ +max-frame-size-limit+)
                                 (+ +max-frame-size-limit+ 1000)))
                      :shrinker (lambda (n) (list (1+ +max-frame-size-limit+))))
      "SETTINGS MAX_FRAME_SIZE > 2^24-1 is PROTOCOL_ERROR")
  (ok (check-property "settings-initial-window-too-large"
                      (lambda (v)
                        (multiple-value-bind (code goaway)
                            (process-error-code
                             (make-settings-frame
                              (list (cons +settings-initial-window-size+ v))))
                          (and goaway (eql code +flow-control-error+))))
                      (lambda (rng size)
                        (declare (ignore size))
                        (rng-int rng (1+ +max-window-size+)
                                 (+ +max-window-size+ 100)))
                      :shrinker (lambda (n) (list (1+ +max-window-size+))))
      "SETTINGS INITIAL_WINDOW_SIZE > 2^31-1 is FLOW_CONTROL_ERROR"))

(deftest prop-pad-overflow
  (ok (check-property "padded-data-overflow"
                      (lambda (pad)
                        (let ((code nil)
                              (conn (make-http2-connection
                                     :on-error (lambda (c d)
                                                 (declare (ignore d))
                                                 (setf code c)))))
                          (connection-process-frame
                           conn
                           (make-headers-frame
                            1
                            (hpack-encode-headers (make-hpack-context) nil)
                            :end-headers t))
                          (setf code nil)
                          (let ((payload
                                  (if (zerop pad)
                                      (make-array 0 :element-type '(unsigned-byte 8))
                                      (make-array 1 :element-type '(unsigned-byte 8)
                                                  :initial-element pad))))
                            (connection-process-frame
                             conn
                             (make-frame :type +frame-data+
                                         :flags +flag-padded+
                                         :stream-id 1
                                         :payload payload)))
                          (and (http2-connection-goaway-sent conn)
                               (eql code +protocol-error+))))
                      (lambda (rng size)
                        (declare (ignore size))
                        (rng-int rng 0 255))
                      :shrinker #'shrink-int)
      "PADDED DATA with pad overflow is PROTOCOL_ERROR"))

(deftest prop-websocket-legal-masked
  (ok (check-property "ws-masked-text-parses"
                      (lambda (bytes)
                        (multiple-value-bind (res err msgs)
                            (ws-parse-result bytes)
                          (declare (ignore msgs))
                          (and (eq res t) (null err))))
                      #'gen-legal-ws-frame
                      :shrinker #'shrink-vector)
      "masked client data frames parse"))

(deftest prop-websocket-unmasked-illegal
  (ok (check-property "ws-unmasked-must-error"
                      (lambda (payload)
                        (let ((bytes (encode-ws-frame +opcode-text+ payload :mask nil)))
                          (multiple-value-bind (res err msgs)
                              (ws-parse-result bytes)
                            (declare (ignore msgs))
                            (and (eq res :error) err))))
                      #'gen-ws-payload
                      :shrinker #'shrink-vector)
      "unmasked client frames are protocol errors"))

(deftest prop-websocket-oversize-control
  (ok (check-property "ws-oversize-control"
                      (lambda (n)
                        (let* ((payload (make-array n :element-type '(unsigned-byte 8)
                                                    :initial-element 0))
                               (bytes (encode-ws-frame +opcode-ping+ payload :mask t)))
                          (multiple-value-bind (res err msgs)
                              (ws-parse-result bytes)
                            (declare (ignore msgs))
                            (and (eq res :error) err))))
                      (lambda (rng size)
                        (declare (ignore size))
                        (rng-int rng 126 200))
                      :shrinker (lambda (n) (when (> n 126) (list 126))))
      "control frames > 125 octets are protocol errors"))

(deftest prop-websocket-rsv
  (ok (check-property "ws-rsv-must-error"
                      (lambda (rsv)
                        (let ((bytes (encode-ws-frame +opcode-text+
                                                      (make-array 1 :element-type '(unsigned-byte 8)
                                                                  :initial-element 0)
                                                      :mask t :rsv rsv)))
                          (multiple-value-bind (res err msgs)
                              (ws-parse-result bytes)
                            (declare (ignore msgs))
                            (and (eq res :error) err))))
                      (lambda (rng size)
                        (declare (ignore size))
                        (rng-choose rng '(#x10 #x20 #x40 #x70)))
                      :shrinker (lambda (r) (list #x10)))
      "RSV bits must be 0"))
