(in-package :cl-user)
(defpackage woo-test.websocket
  (:use :cl :rove)
  (:import-from :woo.websocket
                :websocket-p
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
                :+opcode-pong+)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-bytes-to-string))
(in-package :woo-test.websocket)

;;; Helper functions for testing

(defun bytes-to-list (byte-array)
  "Convert byte array to list for easier comparison."
  (coerce byte-array 'list))

(defun make-test-env (headers)
  "Create a mock environment plist for websocket-p testing."
  (let ((hash-table (make-hash-table :test 'equal)))
    (loop for (key value) on headers by #'cddr
          do (setf (gethash key hash-table) value))
    (list :request-method :GET
          :headers hash-table)))

;;; Test Suite 1: compute-accept-key - RFC 6455 Section 1.3
(deftest test-compute-accept-key
  (testing "RFC 6455 test vector"
    ;; The example from RFC 6455 Section 1.3
    (let ((client-key "dGhlIHNhbXBsZSBub25jZQ==")
          (expected "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="))
      (ok (string= (compute-accept-key client-key) expected)
          "Should produce correct accept key for RFC test vector")))

  (testing "Additional test vectors"
    ;; Additional known vectors for robustness
    (let ((key1 "x3JJHMbDL1EzLkh9GBhXDw==")
          (expected1 "HSmrc0sMlYUkAGmm5OPpG2HaGWk="))
      (ok (string= (compute-accept-key key1) expected1)
          "Should handle another valid WebSocket key")))

  (testing "Another test vector"
    (let* ((key2 "dGVzdA==")
           (result (compute-accept-key key2)))
      ;; Just verify it returns a valid base64 string
      (ok (> (length result) 0)
          "Should return non-empty result for any input")
      (ok (not (find #\Space result))
          "Result should not contain spaces")))

  (testing "Empty and edge cases"
    (let* ((empty-key "")
           (result (compute-accept-key empty-key)))
      (ok (stringp result)
          "Should handle empty string without error")
      (ok (> (length result) 0)
          "Empty key should still produce a hash"))))

;;; Test Suite 2: Opcode constants
(deftest test-opcode-constants
  (testing "Opcode values match RFC 6455 Section 5.2"
    (ok (= +opcode-continuation+ #x0)
        "Continuation opcode should be 0x0")
    (ok (= +opcode-text+ #x1)
        "Text opcode should be 0x1")
    (ok (= +opcode-binary+ #x2)
        "Binary opcode should be 0x2")
    (ok (= +opcode-close+ #x8)
        "Close opcode should be 0x8")
    (ok (= +opcode-ping+ #x9)
        "Ping opcode should be 0x9")
    (ok (= +opcode-pong+ #xA)
        "Pong opcode should be 0xA")))

;;; Test Suite 3: websocket-p detection
(deftest test-websocket-p
  (testing "Valid WebSocket upgrade request"
    (let ((env (make-test-env
                '("upgrade" "websocket"
                  "connection" "Upgrade"
                  "sec-websocket-key" "dGhlIHNhbXBsZSBub25jZQ=="
                  "sec-websocket-version" "13"))))
      (ok (websocket-p env)
          "Should detect valid WebSocket upgrade request")))

  (testing "Valid with mixed-case connection header"
    (let ((env (make-test-env
                '("upgrade" "WebSocket"
                  "connection" "keep-alive, Upgrade"
                  "sec-websocket-key" "x3JJHMbDL1EzLkh9GBhXDw=="
                  "sec-websocket-version" "13"))))
      (ok (websocket-p env)
          "Should handle 'upgrade' in connection header with other values")))

  (testing "Invalid: missing upgrade header"
    (let ((env (make-test-env
                '("connection" "Upgrade"
                  "sec-websocket-key" "dGhlIHNhbXBsZSBub25jZQ=="
                  "sec-websocket-version" "13"))))
      (ok (not (websocket-p env))
          "Should reject request without upgrade header")))

  (testing "Invalid: missing connection header"
    (let ((env (make-test-env
                '("upgrade" "websocket"
                  "sec-websocket-key" "dGhlIHNhbXBsZSBub25jZQ=="
                  "sec-websocket-version" "13"))))
      (ok (not (websocket-p env))
          "Should reject request without connection header")))

  (testing "Invalid: missing sec-websocket-key"
    (let ((env (make-test-env
                '("upgrade" "websocket"
                  "connection" "Upgrade"
                  "sec-websocket-version" "13"))))
      (ok (not (websocket-p env))
          "Should reject request without WebSocket key")))

  (testing "Invalid: wrong WebSocket version"
    (let ((env (make-test-env
                '("upgrade" "websocket"
                  "connection" "Upgrade"
                  "sec-websocket-key" "dGhlIHNhbXBsZSBub25jZQ=="
                  "sec-websocket-version" "12"))))
      (ok (not (websocket-p env))
          "Should reject request with wrong WebSocket version")))

  (testing "Invalid: wrong HTTP method"
    (let ((env (list :request-method :POST
                     :headers (make-hash-table :test 'equal))))
      (setf (gethash "upgrade" (getf env :headers)) "websocket"
            (gethash "connection" (getf env :headers)) "Upgrade"
            (gethash "sec-websocket-key" (getf env :headers)) "dGhlIHNhbXBsZSBub25jZQ=="
            (gethash "sec-websocket-version" (getf env :headers)) "13")
      (ok (not (websocket-p env))
          "Should reject non-GET requests")))

  (testing "Invalid: wrong upgrade value"
    (let ((env (make-test-env
                '("upgrade" "http/2.0"
                  "connection" "Upgrade"
                  "sec-websocket-key" "dGhlIHNhbXBsZSBub25jZQ=="
                  "sec-websocket-version" "13"))))
      (ok (not (websocket-p env))
          "Should reject non-websocket upgrade"))))

;;; Test Suite 4: Frame construction via make-frame (internal symbol)
(deftest test-make-frame-small-payload
  (testing "Text frame with small payload (<126 bytes)"
    (let* ((payload (string-to-utf-8-bytes "Hello"))
           (frame (woo.websocket::make-frame +opcode-text+ payload :fin t :mask nil)))
      (ok (= (length frame) (+ 2 5))
          "Frame should have 2-byte header + 5-byte payload")
      ;; First byte: FIN=1, opcode=1
      (ok (= (aref frame 0) #x81)
          "First byte should be 0x81 (FIN=1, opcode=text)")
      ;; Second byte: MASK=0, length=5
      (ok (= (aref frame 1) 5)
          "Second byte should be 5 (unmasked, length=5)")
      ;; Payload
      (ok (equalp (subseq frame 2) payload)
          "Payload should match original data")))

  (testing "Binary frame with small payload"
    (let* ((payload (make-array 10 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3 4 5 6 7 8 9 10)))
           (frame (woo.websocket::make-frame +opcode-binary+ payload :fin t :mask nil)))
      (ok (= (length frame) 12)
          "Frame should be 2 + 10 bytes")
      (ok (= (aref frame 0) #x82)
          "First byte should be 0x82 (FIN=1, opcode=binary)")
      (ok (= (aref frame 1) 10)
          "Second byte should be 10")
      (ok (equalp (subseq frame 2) payload)
          "Binary payload should match")))

  (testing "Frame without FIN bit"
    (let* ((payload (string-to-utf-8-bytes "fragment"))
           (frame (woo.websocket::make-frame +opcode-text+ payload :fin nil :mask nil)))
      (ok (= (aref frame 0) #x01)
          "First byte should be 0x01 (FIN=0, opcode=text)")
      (ok (= (aref frame 1) 8)
          "Payload length should be 8"))))

(deftest test-make-frame-medium-payload
  (testing "Text frame with medium payload (126-65535 bytes)"
    (let* ((payload (make-array 200 :element-type '(unsigned-byte 8)
                                :initial-element (char-code #\A)))
           (frame (woo.websocket::make-frame +opcode-text+ payload :fin t :mask nil)))
      (ok (= (length frame) (+ 4 200))
          "Frame should have 4-byte header + 200-byte payload")
      (ok (= (aref frame 0) #x81)
          "First byte should be 0x81")
      (ok (= (aref frame 1) 126)
          "Second byte should be 126 (extended length indicator)")
      ;; Extended length in network byte order (big-endian)
      (ok (= (aref frame 2) 0)
          "Extended length high byte should be 0")
      (ok (= (aref frame 3) 200)
          "Extended length low byte should be 200")
      (ok (equalp (subseq frame 4) payload)
          "Payload should match")))

  (testing "Edge case: payload length exactly 126"
    (let* ((payload (make-array 126 :element-type '(unsigned-byte 8)
                                :initial-element 42))
           (frame (woo.websocket::make-frame +opcode-binary+ payload :fin t :mask nil)))
      (ok (= (aref frame 1) 126)
          "Should use extended length for exactly 126 bytes")
      (ok (= (length frame) (+ 4 126))
          "Total frame length should be 130")))

  (testing "Maximum medium frame: 65535 bytes"
    (let* ((payload (make-array 65535 :element-type '(unsigned-byte 8)
                                :initial-element 1))
           (frame (woo.websocket::make-frame +opcode-binary+ payload :fin t :mask nil)))
      (ok (= (aref frame 1) 126)
          "Should use 16-bit extended length")
      (ok (= (aref frame 2) 255)
          "High byte should be 255")
      (ok (= (aref frame 3) 255)
          "Low byte should be 255")
      (ok (= (length frame) (+ 4 65535))
          "Total frame length correct"))))

(deftest test-make-frame-large-payload
  (testing "Large frame (>65535 bytes) uses 64-bit length"
    (let* ((size 70000)
           (payload (make-array size :element-type '(unsigned-byte 8)
                                :initial-element 99))
           (frame (woo.websocket::make-frame +opcode-binary+ payload :fin t :mask nil)))
      (ok (= (aref frame 1) 127)
          "Second byte should be 127 (64-bit length indicator)")
      (ok (= (length frame) (+ 10 size))
          "Frame should have 10-byte header + payload")
      ;; Verify 64-bit length encoding (big-endian)
      (let ((decoded-len 0))
        (loop for i from 2 to 9
              for shift from 56 downto 0 by 8
              do (setf decoded-len (logior decoded-len
                                           (ash (aref frame i) shift))))
        (ok (= decoded-len size)
            "Decoded 64-bit length should match payload size")))))

(deftest test-make-frame-control-frames
  (testing "Ping frame"
    (let* ((payload (string-to-utf-8-bytes "ping"))
           (frame (woo.websocket::make-frame +opcode-ping+ payload :fin t :mask nil)))
      (ok (= (aref frame 0) #x89)
          "First byte should be 0x89 (FIN=1, opcode=ping)")
      (ok (= (aref frame 1) 4)
          "Payload length should be 4")))

  (testing "Pong frame"
    (let* ((payload (string-to-utf-8-bytes "pong"))
           (frame (woo.websocket::make-frame +opcode-pong+ payload :fin t :mask nil)))
      (ok (= (aref frame 0) #x8A)
          "First byte should be 0x8A (FIN=1, opcode=pong)")))

  (testing "Close frame with status code"
    (let* ((payload (make-array 2 :element-type '(unsigned-byte 8)
                                :initial-contents '(#x03 #xE8))) ; 1000 in big-endian
           (frame (woo.websocket::make-frame +opcode-close+ payload :fin t :mask nil)))
      (ok (= (aref frame 0) #x88)
          "First byte should be 0x88 (FIN=1, opcode=close)")
      (ok (= (aref frame 1) 2)
          "Payload length should be 2")
      (ok (= (aref frame 2) #x03)
          "Status code high byte")
      (ok (= (aref frame 3) #xE8)
          "Status code low byte")))

  (testing "Empty close frame"
    (let* ((payload (make-array 0 :element-type '(unsigned-byte 8)))
           (frame (woo.websocket::make-frame +opcode-close+ payload :fin t :mask nil)))
      (ok (= (length frame) 2)
          "Empty close frame should be 2 bytes")
      (ok (= (aref frame 0) #x88)
          "Should have close opcode")
      (ok (= (aref frame 1) 0)
          "Payload length should be 0"))))

(deftest test-make-frame-masking
  (testing "Client frame with masking"
    (let* ((payload (string-to-utf-8-bytes "test"))
           (frame (woo.websocket::make-frame +opcode-text+ payload :fin t :mask t)))
      (ok (= (length frame) (+ 2 4 4))
          "Masked frame should have header + mask key + payload")
      (ok (= (logand (aref frame 1) #x80) #x80)
          "MASK bit should be set in second byte")
      ;; Verify masking key exists (4 bytes)
      (ok (>= (length frame) 10)
          "Frame should contain masking key")
      ;; Payload is masked, so it won't match original
      (ok (not (equalp (subseq frame 6) payload))
          "Masked payload should differ from original"))))

;;; Test Suite 5: ws-state structure (internal)
(deftest test-ws-state-creation
  (testing "Create ws-state with defaults"
    (let ((state (woo.websocket::make-ws-state)))
      (ok (woo.websocket::ws-state-p state)
          "Should create valid ws-state")
      (ok (vectorp (woo.websocket::ws-state-buffer state))
          "Buffer should be a vector")
      (ok (= (length (woo.websocket::ws-state-buffer state)) 0)
          "Buffer should start empty")
      (ok (null (woo.websocket::ws-state-fragment-opcode state))
          "Fragment opcode should be nil")
      (ok (vectorp (woo.websocket::ws-state-fragment-buffer state))
          "Fragment buffer should be a vector")
      (ok (null (woo.websocket::ws-state-socket state))
          "Socket should be nil by default")))

  (testing "Create ws-state with callbacks"
    (let* ((msg-called nil)
           (ping-called nil)
           (state (woo.websocket::make-ws-state
                   :on-message (lambda (opcode payload)
                                 (declare (ignore opcode payload))
                                 (setf msg-called t))
                   :on-ping (lambda (payload)
                              (declare (ignore payload))
                              (setf ping-called t)))))
      (ok (functionp (woo.websocket::ws-state-on-message state))
          "on-message should be a function")
      (ok (functionp (woo.websocket::ws-state-on-ping state))
          "on-ping should be a function")
      ;; Test callback invocation
      (funcall (woo.websocket::ws-state-on-message state) +opcode-text+ #())
      (ok msg-called
          "Should be able to invoke on-message callback")
      (funcall (woo.websocket::ws-state-on-ping state) #())
      (ok ping-called
          "Should be able to invoke on-ping callback"))))

;;; Test Suite 6: Close frame payload construction
(deftest test-close-frame-construction
  (testing "Close frame with status code 1000 and reason"
    ;; This tests the helper logic that would be used in send-close
    (let* ((code 1000)
           (reason "Normal closure")
           (reason-bytes (string-to-utf-8-bytes reason))
           (payload (make-array (+ 2 (length reason-bytes))
                                :element-type '(unsigned-byte 8))))
      ;; Construct payload: 2 bytes for status code + UTF-8 reason
      (setf (aref payload 0) (ldb (byte 8 8) code)  ; High byte
            (aref payload 1) (ldb (byte 8 0) code)) ; Low byte
      (replace payload reason-bytes :start1 2)

      (ok (= (length payload) (+ 2 (length reason-bytes)))
          "Payload should be status code + reason")
      (ok (= (aref payload 0) 3)
          "High byte of 1000 should be 3")
      (ok (= (aref payload 1) 232)
          "Low byte of 1000 should be 232 (0xE8)")
      ;; Verify reason can be extracted
      (let ((extracted-reason (utf-8-bytes-to-string payload :start 2)))
        (ok (string= extracted-reason reason)
            "Should be able to extract reason from payload"))))

  (testing "Close frame with different status codes"
    (let ((test-codes '((1001 . "Going Away")
                        (1002 . "Protocol Error")
                        (1003 . "Unsupported Data")
                        (1006 . "Abnormal Closure")
                        (1008 . "Policy Violation")
                        (1011 . "Internal Error"))))
      (loop for (code . reason) in test-codes
            do (let* ((reason-bytes (string-to-utf-8-bytes reason))
                      (payload (make-array (+ 2 (length reason-bytes))
                                           :element-type '(unsigned-byte 8))))
                 (setf (aref payload 0) (ldb (byte 8 8) code)
                       (aref payload 1) (ldb (byte 8 0) code))
                 (replace payload reason-bytes :start1 2)
                 ;; Verify code can be reconstructed
                 (let ((reconstructed-code (+ (ash (aref payload 0) 8)
                                              (aref payload 1))))
                   (ok (= reconstructed-code code)
                       (format nil "Should correctly encode status code ~A" code)))))))

  (testing "Close frame with empty reason"
    (let* ((code 1000)
           (payload (make-array 2 :element-type '(unsigned-byte 8))))
      (setf (aref payload 0) (ldb (byte 8 8) code)
            (aref payload 1) (ldb (byte 8 0) code))
      (ok (= (length payload) 2)
          "Close frame with no reason should be 2 bytes")
      (ok (= (+ (ash (aref payload 0) 8) (aref payload 1)) code)
          "Should correctly encode status code"))))

;;; Test Suite 7: Edge cases and validation
(deftest test-frame-edge-cases
  (testing "Empty payload frame"
    (let* ((payload (make-array 0 :element-type '(unsigned-byte 8)))
           (frame (woo.websocket::make-frame +opcode-text+ payload :fin t :mask nil)))
      (ok (= (length frame) 2)
          "Empty frame should be 2 bytes (just header)")
      (ok (= (aref frame 0) #x81)
          "Should have text opcode with FIN")
      (ok (= (aref frame 1) 0)
          "Payload length should be 0")))

  (testing "Single byte payload"
    (let* ((payload (make-array 1 :element-type '(unsigned-byte 8)
                                :initial-element 65))
           (frame (woo.websocket::make-frame +opcode-binary+ payload :fin t :mask nil)))
      (ok (= (length frame) 3)
          "Single byte frame should be 3 bytes")
      (ok (= (aref frame 1) 1)
          "Payload length should be 1")
      (ok (= (aref frame 2) 65)
          "Payload byte should be preserved")))

  (testing "Payload at boundary: 125 bytes"
    (let* ((payload (make-array 125 :element-type '(unsigned-byte 8)
                                :initial-element 0))
           (frame (woo.websocket::make-frame +opcode-text+ payload :fin t :mask nil)))
      (ok (= (aref frame 1) 125)
          "125 bytes should use single-byte length")
      (ok (= (length frame) (+ 2 125))
          "Should not use extended length")))

  (testing "Payload at boundary: 65536 bytes"
    (let* ((payload (make-array 65536 :element-type '(unsigned-byte 8)
                                :initial-element 1))
           (frame (woo.websocket::make-frame +opcode-binary+ payload :fin t :mask nil)))
      (ok (= (aref frame 1) 127)
          "65536 bytes should use 64-bit length")
      (ok (= (length frame) (+ 10 65536))
          "Should use 64-bit extended length"))))

;;; Test Suite 8: UTF-8 handling
(deftest test-utf8-handling
  (testing "ASCII text in frames"
    (let* ((text "Hello, World!")
           (payload (string-to-utf-8-bytes text))
           (frame (woo.websocket::make-frame +opcode-text+ payload :fin t :mask nil)))
      (ok (string= (utf-8-bytes-to-string (subseq frame 2)) text)
          "ASCII text should round-trip correctly")))

  (testing "Unicode text in frames"
    (let* ((text "Hello, 世界! 🌍")
           (payload (string-to-utf-8-bytes text))
           (frame (woo.websocket::make-frame +opcode-text+ payload :fin t :mask nil)))
      (ok (> (length payload) (length text))
          "UTF-8 encoding should use multiple bytes for Unicode")
      (ok (string= (utf-8-bytes-to-string (subseq frame 2)) text)
          "Unicode text should round-trip correctly")))

  (testing "Empty string"
    (let* ((text "")
           (payload (string-to-utf-8-bytes text))
           (frame (woo.websocket::make-frame +opcode-text+ payload :fin t :mask nil)))
      (ok (= (length payload) 0)
          "Empty string should produce empty payload")
      (ok (= (length frame) 2)
          "Empty text frame should be 2 bytes"))))

;;; Test Suite 9: Opcode validation
(deftest test-opcode-values
  (testing "All opcodes produce valid frames"
    (let ((opcodes (list +opcode-continuation+
                         +opcode-text+
                         +opcode-binary+
                         +opcode-close+
                         +opcode-ping+
                         +opcode-pong+))
          (payload (make-array 5 :element-type '(unsigned-byte 8)
                               :initial-element 42)))
      (loop for opcode in opcodes
            do (let ((frame (woo.websocket::make-frame opcode payload :fin t :mask nil)))
                 (ok (= (logand (aref frame 0) #x0F) opcode)
                     (format nil "Frame should have correct opcode ~X" opcode)))))))

;;; B12: RFC 6455 client-frame rules
(defun apply-mask (payload key)
  (let ((out (make-array (length payload) :element-type '(unsigned-byte 8))))
    (dotimes (i (length payload))
      (setf (aref out i) (logxor (aref payload i) (aref key (mod i 4)))))
    out))

(defun masked-frame (opcode payload &key (fin t) (rsv 0) (len7 nil))
  "Build a masked client frame. LEN7 overrides the 7-bit length field."
  (let* ((payload-len (length payload))
         (key (make-array 4 :element-type '(unsigned-byte 8)
                          :initial-contents '(1 2 3 4)))
         (use-len (or len7 payload-len))
         (ext (cond ((and (null len7) (< payload-len 126)) 0)
                    ((or (eql len7 126) (and (null len7) (< payload-len 65536))) 2)
                    (t 8)))
         (hdr (+ 2 ext 4))
         (frame (make-array (+ hdr payload-len) :element-type '(unsigned-byte 8)))
         (idx 0))
    (setf (aref frame 0) (logior (if fin #x80 0) rsv (logand opcode #x0F)))
    (incf idx)
    (setf (aref frame 1)
          (logior #x80
                  (cond (len7 len7)
                        ((< payload-len 126) payload-len)
                        ((< payload-len 65536) 126)
                        (t 127))))
    (incf idx)
    (case ext
      (2 (setf (aref frame idx) (ldb (byte 8 8) payload-len)
               (aref frame (1+ idx)) (ldb (byte 8 0) payload-len))
         (incf idx 2))
      (8 (loop for i from 7 downto 0
               do (setf (aref frame idx) (ldb (byte 8 (* i 8)) payload-len))
                  (incf idx))))
    (replace frame key :start1 idx)
    (incf idx 4)
    (let ((masked (apply-mask payload key)))
      (replace frame masked :start1 idx))
    frame))

(defun unmasked-frame (opcode payload)
  (woo.websocket::make-frame opcode payload :fin t :mask nil))

(defun parse-bytes (bytes)
  (let* ((err nil)
         (state (woo.websocket::make-ws-state
                 :on-error (lambda (e) (setf err e))))
         (buf (woo.websocket::ws-state-buffer state)))
    (adjust-array buf (length bytes) :fill-pointer (length bytes))
    (replace buf bytes)
    (values (woo.websocket::parse-frame state) err state)))

(deftest test-reject-unmasked-client-frames
  (testing "unmasked client frames are protocol errors"
    (multiple-value-bind (result err)
        (parse-bytes (unmasked-frame +opcode-text+ (string-to-utf-8-bytes "hi")))
      (ok (eq result :error))
      (ok (typep err 'woo.websocket:websocket-protocol-error)))))

(deftest test-reject-rsv-bits
  (testing "RSV bits must be zero"
    (multiple-value-bind (result err)
        (parse-bytes (masked-frame +opcode-text+ (string-to-utf-8-bytes "x") :rsv #x40))
      (ok (eq result :error))
      (ok (typep err 'woo.websocket:websocket-protocol-error)))))

(deftest test-reject-fragmented-control
  (testing "control frames cannot be fragmented"
    (multiple-value-bind (result err)
        (parse-bytes (masked-frame +opcode-ping+ (string-to-utf-8-bytes "ab") :fin nil))
      (ok (eq result :error))
      (ok (typep err 'woo.websocket:websocket-protocol-error)))))

(deftest test-reject-oversize-control
  (testing "control payload > 125 is rejected before 16-bit length"
    (let ((payload (make-array 130 :element-type '(unsigned-byte 8) :initial-element 1)))
      (multiple-value-bind (result err)
          (parse-bytes (masked-frame +opcode-ping+ payload :len7 126))
        (ok (eq result :error))
        (ok (typep err 'woo.websocket:websocket-protocol-error))))))

(deftest test-reject-huge-127-payload
  (testing "opcode 127 with huge length does not allocate"
    (let ((header (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (aref header 0) #x82
            (aref header 1) (logior #x80 127)
            (aref header 2) #x00
            (aref header 3) #x01) ; 2^48-scale length, still over max
      (loop for i from 4 to 9 do (setf (aref header i) #xff))
      (multiple-value-bind (result err)
          (parse-bytes header)
        (ok (eq result :error))
        (ok (typep err 'woo.websocket:websocket-protocol-error))))))

(deftest test-masked-text-accepted
  (testing "masked client text frame parses"
    (let ((got nil)
          (payload (string-to-utf-8-bytes "Hello")))
      (multiple-value-bind (result err state)
          (let* ((state (woo.websocket::make-ws-state
                         :on-message (lambda (op data)
                                       (setf got (list op data)))
                         :on-error (lambda (e) (declare (ignore e)))))
                 (bytes (masked-frame +opcode-text+ payload))
                 (buf (woo.websocket::ws-state-buffer state)))
            (adjust-array buf (length bytes) :fill-pointer (length bytes))
            (replace buf bytes)
            (values (woo.websocket::parse-frame state) nil state))
        (declare (ignore err state))
        (ok (eq result t))
        (ok got)
        (ok (equalp (second got) payload))))))

(deftest test-opcode-values-low-nibble
  (testing "Opcode in first byte low nibble"
    (let* ((payload #())
           (frame (woo.websocket::make-frame +opcode-text+ payload :fin t :mask nil)))
      (ok (= (logand (aref frame 0) #x0F) +opcode-text+)
          "Opcode should be in low 4 bits of first byte")
      (ok (= (logand (aref frame 0) #xF0) #x80)
          "FIN bit should be in high bit of first byte"))))

;;; B1/B2: a rejected frame must not parse as success on retry, and the
;;; reader must stop. Fragmentation, reserved opcodes, and close bodies
;;; fail the connection; the fragment buffer stays within +max-ws-payload+.

(defun ws-octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(defun concat-octets (&rest parts)
  (let* ((n (reduce #'+ parts :key #'length :initial-value 0))
         (out (make-array n :element-type '(unsigned-byte 8)))
         (i 0))
    (dolist (p parts out)
      (replace out p :start1 i)
      (incf i (length p)))))

(defun ws-buffer-copy (state)
  (let ((buf (woo.websocket::ws-state-buffer state)))
    (subseq buf 0 (length buf))))

(defun feed-ws (state bytes)
  (let* ((buf (woo.websocket::ws-state-buffer state))
         (old (length buf))
         (grown (adjust-array buf (+ old (length bytes))
                              :fill-pointer (+ old (length bytes)))))
    (setf (woo.websocket::ws-state-buffer state) grown)
    (replace grown bytes :start1 old))
  (woo.websocket::parse-frame state))

(defun make-parse-state (&key on-message on-ping on-pong on-close on-error)
  (woo.websocket::make-ws-state
   :on-message on-message
   :on-ping on-ping
   :on-pong on-pong
   :on-close on-close
   :on-error on-error))

(defun assert-rejected-stays-rejected (bytes)
  "A rejected frame stays buffered and the next parse is still not T."
  (let* ((errors 0)
         (state (make-parse-state
                 :on-error (lambda (e)
                             (declare (ignore e))
                             (incf errors)))))
    (ok (eq (feed-ws state bytes) :error))
    (let ((buffered (ws-buffer-copy state)))
      (ok (equalp buffered bytes)
          "rejected frame is not consumed")
      (ok (not (eq (woo.websocket::parse-frame state) t))
          "retrying the same buffer must not succeed")
      (ok (eq (woo.websocket::parse-frame state) :error))
      (ok (equalp (ws-buffer-copy state) buffered))
      (ok (= errors 1)
          "a failed connection is not parsed again"))))

(defmacro with-stubbed-close ((closed) &body body)
  "Record CLOSE-SOCKET calls. A bare test socket must not reach the real closer."
  (let ((sym (gensym)) (orig (gensym)))
    `(let* ((,sym (find-symbol "CLOSE-SOCKET" :woo.websocket))
            (,orig (symbol-function ,sym))
            (,closed nil))
       (unwind-protect
            (progn
              (setf (symbol-function ,sym)
                    (lambda (socket)
                      (push socket ,closed)
                      (setf (woo.ev.socket:socket-open-p socket) nil)
                      t))
              ,@body)
         (setf (symbol-function ,sym) ,orig)))))

(defun make-bare-socket ()
  (woo.ev.socket::%make-socket
   :fd 0
   :last-activity 0.0d0
   :open-p t
   :watchers (make-array 3 :initial-element (cffi:null-pointer))))

(deftest test-rejected-frame-is-not-success-on-retry
  (testing "unmasked client frame"
    (assert-rejected-stays-rejected
     (unmasked-frame +opcode-text+ (string-to-utf-8-bytes "nope"))))
  (testing "reserved opcode"
    (assert-rejected-stays-rejected
     (masked-frame #x3 (string-to-utf-8-bytes "x"))))
  (testing "one-octet close body"
    (assert-rejected-stays-rejected
     (masked-frame +opcode-close+ (ws-octets 1)))))

(deftest test-setup-websocket-stops-on-rejected-frame
  (testing "reader returns, closes the socket, and does not parse again"
    (with-stubbed-close (closed)
      (let* ((errors 0)
             (messages nil)
             (socket (make-bare-socket))
             (bad (unmasked-frame +opcode-text+ (string-to-utf-8-bytes "spin")))
             (good (masked-frame +opcode-text+ (string-to-utf-8-bytes "ok")))
             (state (setup-websocket
                     socket
                     :on-message (lambda (op data)
                                   (push (list op (copy-seq data)) messages))
                     :on-ping (lambda (payload) (declare (ignore payload)))
                     :on-close (lambda (code reason)
                                 (declare (ignore code reason)))
                     :on-error (lambda (e)
                                 (declare (ignore e))
                                 (incf errors))))
             (reader (woo.ev.socket:socket-data socket))
             (stopped t))
        #+sbcl
        (handler-case
            (sb-ext:with-timeout 1
              (funcall reader bad)
              (funcall reader good))
          (sb-ext:timeout ()
            (setf stopped nil)))
        #-sbcl
        (progn
          (funcall reader bad)
          (funcall reader good))
        (ok stopped "setup-websocket stops after a rejected frame")
        (when stopped
          (ok (= errors 1))
          (ok closed "socket was closed")
          (ok (not (woo.ev.socket:socket-open-p socket)))
          (ok (null messages) "rejected frame is not a message")
          (ok (equalp (ws-buffer-copy state) bad)
              "later bytes are not parsed onto the rejected frame")
          (ok (woo.websocket::ws-state-failed state))
          (ok (not (eq (woo.websocket::parse-frame state) t)))
          (ok (eq (woo.websocket::parse-frame state) :error))))))
  (testing "T still consumes every complete frame, NIL waits"
    (with-stubbed-close (closed)
      (let* ((messages nil)
             (socket (make-bare-socket))
             (hello (masked-frame +opcode-text+ (string-to-utf-8-bytes "Hello")))
             (state (setup-websocket
                     socket
                     :on-message (lambda (op data)
                                   (push (list op (copy-seq data)) messages))
                     :on-ping (lambda (payload) (declare (ignore payload)))
                     :on-close (lambda (code reason)
                                 (declare (ignore code reason)))))
             (reader (woo.ev.socket:socket-data socket)))
        (funcall reader (concat-octets
                         (masked-frame +opcode-text+ (string-to-utf-8-bytes "A"))
                         (masked-frame +opcode-text+ (string-to-utf-8-bytes "B"))))
        (ok (= (length messages) 2))
        (ok (null closed))
        (funcall reader (subseq hello 0 4))
        (ok (= (length messages) 2))
        (ok (woo.ev.socket:socket-open-p socket))
        (funcall reader (subseq hello 4))
        (ok (= (length messages) 3))
        (ok (equalp (second (first messages)) (string-to-utf-8-bytes "Hello")))
        (ok (null (woo.websocket::ws-state-failed state))))))
  (testing "a good frame followed by a rejected one stops before the next frame"
    (with-stubbed-close (closed)
      (let* ((messages nil)
             (socket (make-bare-socket))
             (state (setup-websocket
                     socket
                     :on-message (lambda (op data)
                                   (push (list op (copy-seq data)) messages))
                     :on-ping (lambda (payload) (declare (ignore payload)))
                     :on-close (lambda (code reason)
                                 (declare (ignore code reason)))))
             (reader (woo.ev.socket:socket-data socket)))
        (funcall reader
                 (concat-octets
                  (masked-frame +opcode-text+ (string-to-utf-8-bytes "A"))
                  (unmasked-frame +opcode-text+ (string-to-utf-8-bytes "bad"))
                  (masked-frame +opcode-text+ (string-to-utf-8-bytes "C"))))
        (ok (= (length messages) 1))
        (ok (equalp (second (first messages)) (string-to-utf-8-bytes "A")))
        (ok closed)
        (ok (woo.websocket::ws-state-failed state))
        (ok (not (eq (woo.websocket::parse-frame state) t)))
        (ok (eq (woo.websocket::parse-frame state) :error))))))

(deftest test-ws-data-fragment-state-machine
  (testing "fragmented text reassembles and keeps the original opcode"
    (let* ((messages nil)
           (state (make-parse-state
                   :on-message (lambda (op data)
                                 (push (list op (copy-seq data)) messages)))))
      (ok (eq (feed-ws state (masked-frame +opcode-text+
                                           (string-to-utf-8-bytes "He")
                                           :fin nil))
              t))
      (ok (null messages))
      (ok (eq (feed-ws state (masked-frame +opcode-continuation+
                                           (string-to-utf-8-bytes "l")
                                           :fin nil))
              t))
      (ok (null messages))
      (ok (eq (feed-ws state (masked-frame +opcode-continuation+
                                           (string-to-utf-8-bytes "lo")))
              t))
      (ok (= (length messages) 1))
      (ok (= (first (first messages)) +opcode-text+))
      (ok (equalp (second (first messages)) (string-to-utf-8-bytes "Hello")))
      (ok (null (woo.websocket::ws-state-fragment-opcode state)))))
  (testing "binary fragments keep the binary opcode"
    (let* ((messages nil)
           (state (make-parse-state
                   :on-message (lambda (op data)
                                 (push (list op (copy-seq data)) messages)))))
      (ok (eq (feed-ws state (masked-frame +opcode-binary+ (ws-octets 1 2) :fin nil))
              t))
      (ok (eq (feed-ws state (masked-frame +opcode-continuation+ (ws-octets 3)))
              t))
      (ok (equalp (first messages) (list +opcode-binary+ (ws-octets 1 2 3))))))
  (testing "continuation with no open fragment fails the connection"
    (assert-rejected-stays-rejected
     (masked-frame +opcode-continuation+ (string-to-utf-8-bytes "lo"))))
  (testing "a new data opcode while fragmented fails the connection"
    (let* ((messages nil)
           (state (make-parse-state
                   :on-message (lambda (op data)
                                 (push (list op (copy-seq data)) messages)))))
      (ok (eq (feed-ws state (masked-frame +opcode-text+
                                           (string-to-utf-8-bytes "aa")
                                           :fin nil))
              t))
      (let ((bad (masked-frame +opcode-text+ (string-to-utf-8-bytes "bb") :fin nil)))
        (ok (eq (feed-ws state bad) :error))
        (ok (null messages))
        (ok (equalp (ws-buffer-copy state) bad))
        (ok (not (eq (woo.websocket::parse-frame state) t)))
        (ok (eq (woo.websocket::parse-frame state) :error)))))
  (testing "control frames may arrive between fragments"
    (let* ((messages nil)
           (pings nil)
           (state (make-parse-state
                   :on-message (lambda (op data)
                                 (push (list op (copy-seq data)) messages))
                   :on-ping (lambda (payload)
                              (push (copy-seq payload) pings)))))
      (ok (eq (feed-ws state (masked-frame +opcode-text+
                                           (string-to-utf-8-bytes "AB")
                                           :fin nil))
              t))
      (ok (eq (feed-ws state (masked-frame +opcode-ping+ (string-to-utf-8-bytes "Z")))
              t))
      (ok (eq (feed-ws state (masked-frame +opcode-continuation+
                                           (string-to-utf-8-bytes "CD")))
              t))
      (ok (equalp (first pings) (string-to-utf-8-bytes "Z")))
      (ok (equalp (first messages)
                  (list +opcode-text+ (string-to-utf-8-bytes "ABCD")))))))

(deftest test-ws-reserved-opcodes
  (testing "non-control and control reserved opcodes fail"
    (dolist (opcode '(#x3 #x4 #x5 #x6 #x7 #xB #xC #xD #xE #xF))
      (assert-rejected-stays-rejected
       (masked-frame opcode (ws-octets 9)))))
  (testing "fragmented reserved control opcode is still reserved"
    (assert-rejected-stays-rejected
     (masked-frame #xB (ws-octets 1) :fin nil))))

(deftest test-ws-close-body-length
  (testing "empty close is status 1000"
    (let* ((got nil)
           (state (make-parse-state
                   :on-close (lambda (code reason) (setf got (list code reason))))))
      (ok (eq (feed-ws state (masked-frame +opcode-close+
                                           (make-array 0 :element-type '(unsigned-byte 8))))
              t))
      (ok (equal got '(1000 "")))))
  (testing "two-octet close keeps the status and reason"
    (let* ((got nil)
           (reason (string-to-utf-8-bytes "Bye"))
           (payload (make-array (+ 2 (length reason)) :element-type '(unsigned-byte 8)))
           (state (make-parse-state
                   :on-close (lambda (code text) (setf got (list code text))))))
      (setf (aref payload 0) #x03
            (aref payload 1) #xE8)
      (replace payload reason :start1 2)
      (ok (eq (feed-ws state (masked-frame +opcode-close+ payload)) t))
      (ok (equal got '(1000 "Bye")))))
  (testing "one-octet close body fails and does not call on-close"
    (let* ((closed nil)
           (state (make-parse-state
                   :on-close (lambda (code reason)
                               (declare (ignore code reason))
                               (setf closed t)))))
      (ok (eq (feed-ws state (masked-frame +opcode-close+ (ws-octets 9))) :error))
      (ok (null closed))
      (ok (not (eq (woo.websocket::parse-frame state) t)))
      (ok (eq (woo.websocket::parse-frame state) :error)))))

(deftest test-ws-fragment-buffer-cap
  (testing "accumulated fragments may reach +max-ws-payload+ but not pass it"
    (let* ((max woo.websocket:+max-ws-payload+)
           (delivered nil)
           (state (make-parse-state
                   :on-message (lambda (op data)
                                 (declare (ignore op))
                                 (setf delivered (length data))))))
      (setf (woo.websocket::ws-state-fragment-opcode state) +opcode-text+)
      (setf (woo.websocket::ws-state-fragment-buffer state)
            (make-array (1- max)
                        :element-type '(unsigned-byte 8)
                        :adjustable t
                        :fill-pointer (1- max)))
      (ok (eq (feed-ws state (masked-frame +opcode-continuation+ (ws-octets 7)))
              t))
      (ok (= delivered max))
      (ok (null (woo.websocket::ws-state-failed state)))))
  (testing "one octet past the cap fails without growing or succeeding on retry"
    (let* ((max woo.websocket:+max-ws-payload+)
           (messages nil)
           (state (make-parse-state
                   :on-message (lambda (op data)
                                 (declare (ignore op data))
                                 (setf messages t)))))
      (setf (woo.websocket::ws-state-fragment-opcode state) +opcode-binary+)
      (setf (woo.websocket::ws-state-fragment-buffer state)
            (make-array max
                        :element-type '(unsigned-byte 8)
                        :adjustable t
                        :fill-pointer max))
      (let ((before (length (woo.websocket::ws-state-fragment-buffer state))))
        (ok (eq (feed-ws state (masked-frame +opcode-continuation+ (ws-octets 1)))
                :error))
        (ok (= (length (woo.websocket::ws-state-fragment-buffer state)) before))
        (ok (null messages))
        (ok (not (eq (woo.websocket::parse-frame state) t)))
        (ok (eq (woo.websocket::parse-frame state) :error))))))

;;; Frames are consumed before callbacks run, so an exception cannot cause
;;; redelivery. Invalid UTF-8 fails with 1007, bad close codes with 1002,
;;; and a callback error with 1011.

(defmacro with-recorded-close-frames ((sent) &body body)
  "Record (SOCKET CODE) for every close frame the connection would send."
  (let ((sym (gensym)) (orig (gensym)))
    `(let* ((,sym (find-symbol "CLOSE-AFTER-FRAME" :woo.websocket))
            (,orig (symbol-function ,sym))
            (,sent nil))
       (unwind-protect
            (progn
              (setf (symbol-function ,sym)
                    (lambda (socket code)
                      (push (list socket code) ,sent)
                      (when socket
                        (setf (woo.ev.socket:socket-open-p socket) nil))))
              ,@body)
         (setf (symbol-function ,sym) ,orig)))))

(defun close-frame-payload (code &rest reason-octets)
  (apply #'ws-octets (ldb (byte 8 8) code) (ldb (byte 8 0) code) reason-octets))

(defun make-reader-state (socket &key on-message on-close on-ping)
  "SETUP-WEBSOCKET with counters. Returns (values state reader errors-fn)."
  (let* ((errors nil)
         (state (setup-websocket
                 socket
                 :on-message on-message
                 :on-ping (or on-ping (lambda (payload) (declare (ignore payload))))
                 :on-close on-close
                 :on-error (lambda (e) (push e errors)))))
    (values state
            (woo.ev.socket:socket-data socket)
            (lambda () errors))))

(deftest test-ws-invalid-utf-8-close-reason
  (testing "close reason FF FE fails with 1007 once and is not re-processed"
    (with-recorded-close-frames (sent)
      (let* ((closes 0)
             (messages nil)
             (socket (make-bare-socket)))
        (multiple-value-bind (state reader errors)
            (make-reader-state socket
                               :on-message (lambda (op data)
                                             (push (list op data) messages))
                               :on-close (lambda (code reason)
                                           (declare (ignore code reason))
                                           (incf closes)))
          (funcall reader (masked-frame +opcode-close+
                                        (close-frame-payload 1000 #xFF #xFE)))
          (funcall reader (masked-frame +opcode-text+ (string-to-utf-8-bytes "later")))
          (ok (= closes 0) "on-close is not called with an undecodable reason")
          (ok (null messages))
          (ok (= (length (funcall errors)) 1) "on-error runs exactly once")
          (let ((e (first (funcall errors))))
            (ok (typep e 'woo.websocket:websocket-protocol-error))
            (ok (and (typep e 'woo.websocket:websocket-protocol-error)
                     (= (woo.websocket:websocket-protocol-error-code e) 1007))))
          (ok (woo.websocket::ws-state-failed state))
          (ok (eql (woo.websocket::ws-state-close-code state) 1007))
          (ok (equal (mapcar #'second sent) '(1007)) "close frame carries 1007"))))))

(deftest test-ws-callback-error-does-not-redeliver
  (testing "an on-message error fails with 1011 and the frame is not redelivered"
    (with-recorded-close-frames (sent)
      (let* ((calls 0)
             (socket (make-bare-socket)))
        (multiple-value-bind (state reader errors)
            (make-reader-state socket
                               :on-message (lambda (op data)
                                             (declare (ignore op data))
                                             (incf calls)
                                             (error "app bug"))
                               :on-close (lambda (code reason)
                                           (declare (ignore code reason))))
          (funcall reader (masked-frame +opcode-text+ (string-to-utf-8-bytes "A")))
          (funcall reader (masked-frame +opcode-text+ (string-to-utf-8-bytes "B")))
          (funcall reader (masked-frame +opcode-text+ (string-to-utf-8-bytes "C")))
          (ok (= calls 1) "the failing message is delivered once")
          (ok (= (length (funcall errors)) 1))
          (ok (typep (first (funcall errors)) 'simple-error)
              "on-error receives the callback's own error")
          (ok (woo.websocket::ws-state-failed state))
          (ok (eql (woo.websocket::ws-state-close-code state) 1011))
          (ok (equal (mapcar #'second sent) '(1011)))
          (ok (not (eq (woo.websocket::parse-frame state) t)))))))
  (testing "an on-close error is not re-run on later reads"
    (with-recorded-close-frames (sent)
      (let* ((calls 0)
             (socket (make-bare-socket)))
        (multiple-value-bind (state reader errors)
            (make-reader-state socket
                               :on-close (lambda (code reason)
                                           (declare (ignore code reason))
                                           (incf calls)
                                           (error "close handler bug")))
          (funcall reader (masked-frame +opcode-close+ (close-frame-payload 1000)))
          (funcall reader (masked-frame +opcode-ping+ (ws-octets 1)))
          (ok (= calls 1))
          (ok (= (length (funcall errors)) 1))
          (ok (eql (woo.websocket::ws-state-close-code state) 1011)))))))

(deftest test-ws-text-utf-8-validation
  (testing "single-frame text with invalid UTF-8 fails with 1007"
    (dolist (bad (list (ws-octets #xFF)
                       (ws-octets #xC0 #xAF)              ; overlong
                       (ws-octets #xED #xA0 #x80)         ; surrogate
                       (ws-octets #xF4 #x90 #x80 #x80)    ; > U+10FFFF
                       (ws-octets #x68 #xE2 #x82)))       ; truncated
      (let* ((messages nil)
             (state (make-parse-state
                     :on-message (lambda (op data) (push (list op data) messages))
                     :on-error (lambda (e) (declare (ignore e))))))
        (ok (eq (feed-ws state (masked-frame +opcode-text+ bad)) :error))
        (ok (null messages))
        (ok (eql (woo.websocket::ws-state-close-code state) 1007)))))
  (testing "binary frames are not UTF-8 checked"
    (let* ((messages nil)
           (state (make-parse-state
                   :on-message (lambda (op data) (push (list op data) messages)))))
      (ok (eq (feed-ws state (masked-frame +opcode-binary+ (ws-octets #xFF #xFE))) t))
      (ok (= (length messages) 1))))
  (testing "a code point split across fragments is valid once reassembled"
    (let* ((messages nil)
           (euro (string-to-utf-8-bytes (string (code-char #x20AC))))
           (state (make-parse-state
                   :on-message (lambda (op data) (push (list op data) messages)))))
      (ok (eq (feed-ws state (masked-frame +opcode-text+ (subseq euro 0 1) :fin nil)) t))
      (ok (eq (feed-ws state (masked-frame +opcode-continuation+ (subseq euro 1))) t))
      (ok (equalp (second (first messages)) euro))))
  (testing "reassembled text that is invalid fails with 1007"
    (let* ((messages nil)
           (state (make-parse-state
                   :on-message (lambda (op data) (push (list op data) messages))
                   :on-error (lambda (e) (declare (ignore e))))))
      (ok (eq (feed-ws state (masked-frame +opcode-text+ (ws-octets #x61) :fin nil)) t))
      (ok (eq (feed-ws state (masked-frame +opcode-continuation+ (ws-octets #xE2 #x82)))
              :error))
      (ok (null messages))
      (ok (eql (woo.websocket::ws-state-close-code state) 1007)))))

(deftest test-ws-fragmented-message-is-a-fresh-copy
  (testing "a retained reassembled payload is not truncated or overwritten"
    (let* ((messages nil)
           (state (make-parse-state
                   :on-message (lambda (op data)
                                 (declare (ignore op))
                                 ;; Retain without copying.
                                 (push data messages)))))
      (feed-ws state (masked-frame +opcode-text+ (string-to-utf-8-bytes "Hel") :fin nil))
      (feed-ws state (masked-frame +opcode-continuation+ (string-to-utf-8-bytes "lo")))
      (feed-ws state (masked-frame +opcode-text+ (string-to-utf-8-bytes "XY") :fin nil))
      (feed-ws state (masked-frame +opcode-continuation+ (string-to-utf-8-bytes "Z")))
      (ok (= (length messages) 2))
      (ok (equalp (second messages) (string-to-utf-8-bytes "Hello"))
          "first message is intact after the next one")
      (ok (equalp (first messages) (string-to-utf-8-bytes "XYZ")))
      (ok (not (eq (first messages) (second messages)))))))

(deftest test-ws-close-code-validation
  (testing "reserved, local-only, and unassigned close codes fail with 1002"
    (dolist (code '(0 999 1004 1005 1006 1015 1016 2000 2999 5000 65535))
      (let* ((closed nil)
             (state (make-parse-state
                     :on-close (lambda (c r) (declare (ignore r)) (setf closed c))
                     :on-error (lambda (e) (declare (ignore e))))))
        (ok (eq (feed-ws state (masked-frame +opcode-close+ (close-frame-payload code)))
                :error)
            (format nil "close code ~D is rejected" code))
        (ok (null closed))
        (ok (eql (woo.websocket::ws-state-close-code state) 1002)))))
  (testing "defined and private-use close codes are accepted"
    (dolist (code '(1000 1001 1002 1003 1007 1008 1009 1010 1011 1012 1013 1014 3000 4999))
      (let* ((closed nil)
             (state (make-parse-state
                     :on-close (lambda (c r) (declare (ignore r)) (setf closed c)))))
        (ok (eq (feed-ws state (masked-frame +opcode-close+ (close-frame-payload code))) t)
            (format nil "close code ~D is accepted" code))
        (ok (eql closed code))))))

(deftest test-ws-default-close-echo
  (testing "the default on-close echoes a valid code"
    (with-recorded-close-frames (sent)
      (let* ((socket (make-bare-socket))
             (state (setup-websocket socket))
             (reader (woo.ev.socket:socket-data socket)))
        (declare (ignore state))
        (funcall reader (masked-frame +opcode-close+ (close-frame-payload 1001)))
        (ok (equal (mapcar #'second sent) '(1001))))))
  (testing "an empty close is echoed as 1000"
    (with-recorded-close-frames (sent)
      (let* ((socket (make-bare-socket))
             (reader (progn (setup-websocket socket)
                            (woo.ev.socket:socket-data socket))))
        (funcall reader (masked-frame +opcode-close+
                                      (make-array 0 :element-type '(unsigned-byte 8))))
        (ok (equal (mapcar #'second sent) '(1000))))))
  (testing "the default on-close never sends 1005, 1006, or 1015"
    (dolist (code '(1005 1006 1015))
      (with-recorded-close-frames (sent)
        (let ((state (setup-websocket (make-bare-socket))))
          (funcall (woo.websocket::ws-state-on-close state) code "")
          (ok (equal (mapcar #'second sent) '(1000))
              (format nil "~D is replaced by 1000" code)))))))

(deftest test-ws-many-small-frames-in-one-read
  (testing "a read holding many tiny frames is parsed in linear time"
    (with-stubbed-close (closed)
      (let* ((n 500000)
             (count 0)
             (one (masked-frame +opcode-binary+ (ws-octets 7)))
             (bytes (make-array (* n (length one)) :element-type '(unsigned-byte 8)))
             (socket (make-bare-socket))
             (state (setup-websocket
                     socket
                     :on-message (lambda (op data)
                                   (declare (ignore op data))
                                   (incf count))
                     :on-ping (lambda (payload) (declare (ignore payload)))
                     :on-close (lambda (code reason) (declare (ignore code reason)))))
             (reader (woo.ev.socket:socket-data socket))
             (finished t))
        (dotimes (i n)
          (replace bytes one :start1 (* i (length one))))
        ;; Shifting the buffer after each frame moves ~n^2/2 * 7 octets
        ;; (~10^12 octets here); consuming by offset finishes well inside this.
        (handler-case
            (sb-ext:with-timeout 5
              (funcall reader bytes))
          (sb-ext:timeout () (setf finished nil)))
        (ok finished "parsing did not time out")
        (ok (= count n) "every frame was delivered")
        (ok (null closed))
        (ok (zerop (length (woo.websocket::ws-state-buffer state))))
        (ok (zerop (woo.websocket::ws-state-read-pos state))))))
  (testing "a partial frame after complete ones is kept and compacted"
    (with-stubbed-close (closed)
      (let* ((messages nil)
             (socket (make-bare-socket))
             (tail (masked-frame +opcode-text+ (string-to-utf-8-bytes "tail")))
             (state (setup-websocket
                     socket
                     :on-message (lambda (op data)
                                   (declare (ignore op))
                                   (push data messages))
                     :on-ping (lambda (payload) (declare (ignore payload)))
                     :on-close (lambda (code reason) (declare (ignore code reason)))))
             (reader (woo.ev.socket:socket-data socket)))
        (funcall reader (concat-octets
                         (masked-frame +opcode-text+ (string-to-utf-8-bytes "a"))
                         (masked-frame +opcode-text+ (string-to-utf-8-bytes "b"))
                         (subseq tail 0 5)))
        (ok (= (length messages) 2))
        (ok (equalp (ws-buffer-copy state) (subseq tail 0 5)))
        (ok (zerop (woo.websocket::ws-state-read-pos state)))
        (funcall reader (subseq tail 5))
        (ok (equalp (first messages) (string-to-utf-8-bytes "tail")))
        (ok (null closed))))))

;;; The closing handshake with queued writes. A fake event loop lets
;;; WITH-ASYNC-WRITING queue into the socket buffer; FAKE-FLUSH then does
;;; what ASYNC-WRITE does once the buffer is written.

(defmacro with-fake-event-loop (() &body body)
  "Bind *EVLOOP* and make starting or stopping a watcher a no-op."
  (let ((start (gensym)) (stop (gensym)))
    `(let ((woo.ev.event-loop:*evloop* :fake-loop)
           (,start (symbol-function 'lev:ev-io-start))
           (,stop (symbol-function 'lev:ev-io-stop)))
       (unwind-protect
            (progn
              (setf (symbol-function 'lev:ev-io-start) (lambda (loop w) (declare (ignore loop w)))
                    (symbol-function 'lev:ev-io-stop) (lambda (loop w) (declare (ignore loop w))))
              ,@body)
         (setf (symbol-function 'lev:ev-io-start) ,start
               (symbol-function 'lev:ev-io-stop) ,stop)))))

(defun queued-octets (socket)
  "Octets written to SOCKET and not yet flushed."
  (fast-io:finish-output-buffer (woo.ev.socket::socket-buffer socket)))

(defun fake-flush (socket)
  "Write out the buffer and run the write callback, as ASYNC-WRITE does.
   Returns the octets written."
  (let ((octets (queued-octets socket)))
    (woo.ev.socket::reset-buffer socket)
    (let ((cb (woo.ev.socket::socket-write-cb socket)))
      (when cb (funcall cb socket)))
    (when (woo.ev.socket:socket-open-p socket)
      (setf (woo.ev.socket::socket-write-cb socket) nil))
    octets))

(defun server-close-frame (code)
  (unmasked-frame +opcode-close+ (close-frame-payload code)))

(deftest test-ws-nothing-follows-a-queued-close
  (testing "a send after a queued echo is dropped and the close still happens"
    (with-fake-event-loop ()
      (with-stubbed-close (closed)
        (let* ((socket (make-bare-socket))
               (reader (progn (setup-websocket socket)
                              (woo.ev.socket:socket-data socket))))
          (funcall reader (masked-frame +opcode-close+ (close-frame-payload 1000)))
          (ok (null (send-text-frame socket "broadcast"))
              "the data frame is dropped")
          (ok (null (send-ping socket)) "a ping is dropped too")
          (ok (woo.ev.socket:socket-open-p socket)
              "the socket stays open until the close is flushed")
          (ok (equalp (fake-flush socket) (server-close-frame 1000))
              "only the close frame is written")
          (ok closed "the pending close survives the later sends")
          (ok (not (woo.ev.socket:socket-open-p socket)))))))
  (testing "send-close twice queues a single close frame"
    (with-fake-event-loop ()
      (with-stubbed-close (closed)
        (let ((socket (make-bare-socket)))
          (setup-websocket socket)
          (ok (send-close socket 1001))
          (ok (null (send-close socket 1000)))
          (ok (null (send-binary-frame socket (ws-octets 1 2))))
          (ok (equalp (fake-flush socket) (server-close-frame 1001)))
          (ok (null closed)
              "a server-initiated close waits for the peer's close"))))))

(deftest test-ws-server-initiated-close-is-not-echoed
  (testing "the peer's close answers ours: no second close, socket closes"
    (with-fake-event-loop ()
      (with-stubbed-close (closed)
        (let* ((socket (make-bare-socket))
               (reader (progn (setup-websocket socket)
                              (woo.ev.socket:socket-data socket))))
          (send-close socket 1000)
          (ok (equalp (fake-flush socket) (server-close-frame 1000)))
          (ok (woo.ev.socket:socket-open-p socket))
          (funcall reader (masked-frame +opcode-close+ (close-frame-payload 1000)))
          (ok (zerop (length (queued-octets socket)))
              "the default on-close does not echo a second close")
          (ok closed "the socket closes once both closes are exchanged")))))
  (testing "a peer close crossing an unflushed close closes after the flush"
    (with-fake-event-loop ()
      (with-stubbed-close (closed)
        (let* ((socket (make-bare-socket))
               (reader (progn (setup-websocket socket)
                              (woo.ev.socket:socket-data socket))))
          (send-close socket 1001)
          (funcall reader (masked-frame +opcode-close+ (close-frame-payload 1000)))
          (ok (null closed) "our close frame is still queued")
          (ok (equalp (fake-flush socket) (server-close-frame 1001)))
          (ok closed))))))
