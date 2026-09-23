(in-package :cl-user)
(defpackage woo-test.prop.properties
  (:use :cl :rove :woo-test.prop)
  (:import-from :woo.http2.hpack
                :make-hpack-context
                :hpack-context-max-dynamic-table-size
                :hpack-context-dynamic-table
                :hpack-context-dynamic-table-size
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
                :+frame-continuation+
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

(defun gen-code-point (rng)
  "A Unicode scalar value from each UTF-8 length class, never a surrogate."
  (case (rng-uint rng 4)
    (0 (rng-int rng #x20 #x7E))
    (1 (rng-int rng #x80 #x7FF))
    (2 (if (rng-bool rng)
           (rng-int rng #x800 #xD7FF)
           (rng-int rng #xE000 #xFFFD)))
    (t (rng-int rng #x10000 #x10FFFF))))

(defun gen-utf8-payload (rng size)
  "Valid UTF-8 octets, as a TEXT message must carry (RFC 6455 §8.1)."
  (let* ((n (rng-int rng 0 (min 20 size)))
         (s (make-string n)))
    (dotimes (i n)
      (setf (char s i) (code-char (gen-code-point rng))))
    (coerce (trivial-utf-8:string-to-utf-8-bytes s)
            '(simple-array (unsigned-byte 8) (*)))))

(defun gen-legal-ws-frame (rng size)
  "TEXT frames carry valid UTF-8; BINARY frames carry arbitrary octets."
  (let* ((opcode (rng-choose rng (list +opcode-text+ +opcode-binary+)))
         (payload (if (= opcode +opcode-text+)
                      (gen-utf8-payload rng size)
                      (gen-ws-payload rng size)))
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
  ;; LET* so the on-error closure captures CODE, not a free variable.
  (let* ((code nil)
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

(defun octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(deftest rfc7541-independent-vectors
  (testing "RFC 7541 C.1 integer encodings (decoder vs published octets)"
    (multiple-value-bind (v n)
        (woo.http2.hpack::hpack-decode-integer (octets #x0a) 0 5)
      (ok (= v 10))
      (ok (= n 1)))
    (multiple-value-bind (v n)
        (woo.http2.hpack::hpack-decode-integer (octets #x1f #x9a #x0a) 0 5)
      (ok (= v 1337))
      (ok (= n 3)))
    (multiple-value-bind (v n)
        (woo.http2.hpack::hpack-decode-integer (octets #x2a) 0 8)
      (ok (= v 42))
      (ok (= n 1))))
  (testing "RFC 7541 C.4 Huffman www.example.com"
    (let* ((enc (octets #xf1 #xe3 #xc2 #xe5 #xf2 #x3a #x6b #xa0 #xab #x90 #xf4 #xff))
           (dec (woo.http2.hpack::huffman-decode-bytes enc 0 (length enc)))
           (want (map 'vector #'char-code "www.example.com")))
      (ok (equalp dec want))))
  (testing "RFC 7541 C.2.1 literal header without Huffman"
    (let* ((block (octets #x40 #x0a #x63 #x75 #x73 #x74 #x6f #x6d #x2d #x6b #x65 #x79
                          #x0d #x63 #x75 #x73 #x74 #x6f #x6d #x2d #x68 #x65 #x61 #x64 #x65 #x72))
           (decoded (hpack-decode-headers (make-hpack-context) block)))
      (ok (equal decoded '(("custom-key" . "custom-header")))))))

(defun table-has-value (ctx value)
  (find value (hpack-context-dynamic-table ctx) :key #'cdr :test #'string=))

(deftest rfc7541-dynamic-table-eviction
  (testing "RFC 7541 C.5 evicts on a fresh table of 256 octets"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 256))
          (c51 (octets
                #x48 #x03 #x33 #x30 #x32 #x58 #x07 #x70 #x72 #x69 #x76 #x61 #x74 #x65 #x61 #x1d
                #x4d #x6f #x6e #x2c #x20 #x32 #x31 #x20 #x4f #x63 #x74 #x20 #x32 #x30 #x31 #x33
                #x20 #x32 #x30 #x3a #x31 #x33 #x3a #x32 #x31 #x20 #x47 #x4d #x54 #x6e #x17 #x68
                #x74 #x74 #x70 #x73 #x3a #x2f #x2f #x77 #x77 #x77 #x2e #x65 #x78 #x61 #x6d #x70
                #x6c #x65 #x2e #x63 #x6f #x6d))
          (c52 (octets #x48 #x03 #x33 #x30 #x37 #xc1 #xc0 #xbf))
          (c53 (octets
                #x88 #xc1 #x61 #x1d #x4d #x6f #x6e #x2c #x20 #x32 #x31 #x20 #x4f #x63 #x74 #x20
                #x32 #x30 #x31 #x33 #x20 #x32 #x30 #x3a #x31 #x33 #x3a #x32 #x32 #x20 #x47 #x4d
                #x54 #xc0 #x5a #x04 #x67 #x7a #x69 #x70 #x77 #x38 #x66 #x6f #x6f #x3d #x41 #x53
                #x44 #x4a #x4b #x48 #x51 #x4b #x42 #x5a #x58 #x4f #x51 #x57 #x45 #x4f #x50 #x49
                #x55 #x41 #x58 #x51 #x57 #x45 #x4f #x49 #x55 #x3b #x20 #x6d #x61 #x78 #x2d #x61
                #x67 #x65 #x3d #x33 #x36 #x30 #x30 #x3b #x20 #x76 #x65 #x72 #x73 #x69 #x6f #x6e
                #x3d #x31)))
      (let ((headers (hpack-decode-headers ctx c51)))
        (ok (= (length headers) 4))
        (ok (string= (cdar headers) "302"))
        (ok (= (hpack-context-dynamic-table-size ctx) 222))
        (ok (= (length (hpack-context-dynamic-table ctx)) 4)))
      (let ((headers (hpack-decode-headers ctx c52)))
        (ok (string= (cdar headers) "307"))
        (ok (= (hpack-context-dynamic-table-size ctx) 222))
        (ok (= (length (hpack-context-dynamic-table ctx)) 4))
        (ok (string= (cdr (aref (hpack-context-dynamic-table ctx) 0)) "307"))
        (ok (not (table-has-value ctx "302")) "C.5.2 evicts :status 302"))
      (let ((headers (hpack-decode-headers ctx c53)))
        (ok (= (length headers) 6))
        (ok (string= (cdar headers) "200"))
        (ok (string= (cdr (car (last headers)))
                     "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"))
        (ok (= (hpack-context-dynamic-table-size ctx) 215))
        (ok (= (length (hpack-context-dynamic-table ctx)) 3))
        (ok (not (table-has-value ctx "307")))
        (ok (not (table-has-value ctx "https://www.example.com"))))))
  (testing "RFC 7541 C.6 evicts by decoded octet length, not Huffman wire length"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 256))
          (c61 (octets
                #x48 #x82 #x64 #x02 #x58 #x85 #xae #xc3 #x77 #x1a #x4b #x61 #x96 #xd0 #x7a #xbe
                #x94 #x10 #x54 #xd4 #x44 #xa8 #x20 #x05 #x95 #x04 #x0b #x81 #x66 #xe0 #x82 #xa6
                #x2d #x1b #xff #x6e #x91 #x9d #x29 #xad #x17 #x18 #x63 #xc7 #x8f #x0b #x97 #xc8
                #xe9 #xae #x82 #xae #x43 #xd3))
          (c62 (octets #x48 #x83 #x64 #x0e #xff #xc1 #xc0 #xbf))
          (c63 (octets
                #x88 #xc1 #x61 #x96 #xd0 #x7a #xbe #x94 #x10 #x54 #xd4 #x44 #xa8 #x20 #x05 #x95
                #x04 #x0b #x81 #x66 #xe0 #x84 #xa6 #x2d #x1b #xff #xc0 #x5a #x83 #x9b #xd9 #xab
                #x77 #xad #x94 #xe7 #x82 #x1d #xd7 #xf2 #xe6 #xc7 #xb3 #x35 #xdf #xdf #xcd #x5b
                #x39 #x60 #xd5 #xaf #x27 #x08 #x7f #x36 #x72 #xc1 #xab #x27 #x0f #xb5 #x29 #x1f
                #x95 #x87 #x31 #x60 #x65 #xc0 #x03 #xed #x4e #xe5 #xb1 #x06 #x3d #x50 #x07)))
      (hpack-decode-headers ctx c61)
      (ok (= (hpack-context-dynamic-table-size ctx) 222)
          "decoded octets sum to 222; Huffman wire length is shorter")
      (hpack-decode-headers ctx c62)
      (ok (not (table-has-value ctx "302")))
      (ok (string= (cdr (aref (hpack-context-dynamic-table ctx) 0)) "307"))
      (let ((headers (hpack-decode-headers ctx c63)))
        (ok (= (length headers) 6))
        (ok (= (hpack-context-dynamic-table-size ctx) 215))
        (ok (= (length (hpack-context-dynamic-table ctx)) 3)))))
  (testing "Non-ASCII value is 42 octets and evicts an entry that character length would keep"
    ;; "a" + U+65E5 U+672C U+8A9E is 32+1+9 = 42. Three characters would be 36.
    ;; A prior 34-octet entry fits beside 36 (70) but not beside 42.
    (let* ((ctx (make-hpack-context :max-dynamic-table-size 70))
           (prior (octets #x40 #x01 #x61 #x01 #x62))
           (block (octets #x40 #x01 #x61 #x09
                          #xE6 #x97 #xA5 #xE6 #x9C #xAC #xE8 #xAA #x9E)))
      (hpack-decode-headers ctx prior)
      (ok (= (hpack-context-dynamic-table-size ctx) 34))
      (let ((headers (hpack-decode-headers ctx block)))
        (ok (= (length headers) 1))
        (ok (equal (map 'list #'char-code (cdar headers))
                   '(#x65E5 #x672C #x8A9E)))
        (ok (= (length (hpack-context-dynamic-table ctx)) 1))
        (ok (= (hpack-context-dynamic-table-size ctx) 42))
        (ok (not (table-has-value ctx "b")))))
    ;; 42 > 41: the insert empties the table and is not stored.
    (let* ((ctx (make-hpack-context :max-dynamic-table-size 41))
           (block (octets #x40 #x01 #x61 #x09
                          #xE6 #x97 #xA5 #xE6 #x9C #xAC #xE8 #xAA #x9E))
           (headers (hpack-decode-headers ctx block)))
      (ok (= (length headers) 1))
      (ok (= (length (hpack-context-dynamic-table ctx)) 0))
      (ok (= (hpack-context-dynamic-table-size ctx) 0))))
  (testing "Oversized insert empties the table and does not insert"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 50))
          (prior (octets #x40 #x01 #x61 #x01 #x62))
          (huge (let ((value (make-array 15 :element-type '(unsigned-byte 8)
                                         :initial-element #x78))
                      (head (octets #x40 #x04 #x6e #x61 #x6d #x65 #x0f)))
                  (let ((out (make-array (+ (length head) 15)
                                         :element-type '(unsigned-byte 8))))
                    (replace out head)
                    (replace out value :start1 (length head))
                    out))))
      (hpack-decode-headers ctx prior)
      (ok (= (length (hpack-context-dynamic-table ctx)) 1))
      (let ((headers (hpack-decode-headers ctx huge)))
        (ok (string= (caar headers) "name"))
        (ok (= (length (cdar headers)) 15))
        (ok (= (length (hpack-context-dynamic-table ctx)) 0))
        (ok (= (hpack-context-dynamic-table-size ctx) 0)))))
  (testing "Size update after a field or above the advertised max is COMPRESSION_ERROR"
    (let ((ctx (make-hpack-context)))
      (ok (signals (hpack-decode-headers ctx (octets #x82 #x20))
                   'hpack-compression-error))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 4096))
      (ok (= (length (hpack-context-dynamic-table ctx)) 0)))
    (let ((ctx (make-hpack-context)))
      (hpack-decode-headers ctx (octets #x40 #x01 #x61 #x01 #x62))
      (ok (signals (hpack-decode-headers ctx (octets #x3F #xE2 #x1F))
                   'hpack-compression-error))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 4096))
      (ok (= (hpack-context-dynamic-table-size ctx) 34))
      (ok (= (length (hpack-context-dynamic-table ctx)) 1)))
    (let ((ctx (make-hpack-context :max-dynamic-table-size 100)))
      (ok (null (hpack-decode-headers ctx (octets #x3F #x45))))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 100))
      (ok (signals (hpack-decode-headers ctx (octets #x3F #x46))
                   'hpack-compression-error))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 100)))
    (let* ((ctx (make-hpack-context))
           (headers (hpack-decode-headers ctx (octets #x20 #x3F #xE1 #x01 #x82))))
      (ok (= (length headers) 1))
      (ok (string= (caar headers) ":method"))
      (ok (string= (cdar headers) "GET"))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 256)))))

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
                             (make-headers-frame 0 (octets) :end-headers t))
                          (and goaway (eql code +protocol-error+))))
                      (lambda (rng size) (declare (ignore rng size)) 0))
      "HEADERS on stream 0 is PROTOCOL_ERROR")
  (ok (check-property "headers-even-client-id-protocol-error"
                      (lambda (sid)
                        (multiple-value-bind (code goaway)
                            (process-error-code
                             (make-headers-frame sid (octets) :end-headers t))
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
                        (let* ((code nil)
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

;;; Random frame sequences against one connection

(defun gen-octets (rng n &optional (byte nil))
  (let ((v (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n v)
      (setf (aref v i) (or byte (rng-int rng 0 255))))))

(defun u32-octets (n)
  (let ((v (make-array 4 :element-type '(unsigned-byte 8))))
    (dotimes (i 4 v)
      (setf (aref v i) (ldb (byte 8 (* 8 (- 3 i))) n)))))

(defun gen-stream-id (rng)
  ;; Mostly a few client ids so streams are reused, reset and closed;
  ;; sometimes 0 or an even id, which are illegal for most frame types.
  (if (< (rng-int rng 0 9) 8)
      (1+ (* 2 (rng-int rng 0 5)))
      (rng-choose rng '(0 2 101))))

(defun gen-header-block (rng)
  (case (rng-int rng 0 7)
    ;; custom-key: custom-header with incremental indexing (RFC 7541 C.2.1)
    (0 (make-array 26 :element-type '(unsigned-byte 8)
                      :initial-contents
                      '(#x40 #x0a #x63 #x75 #x73 #x74 #x6f #x6d #x2d #x6b #x65 #x79
                        #x0d #x63 #x75 #x73 #x74 #x6f #x6d #x2d #x68 #x65 #x61 #x64
                        #x65 #x72)))
    (1 (gen-octets rng (rng-int rng 0 40)))           ; usually undecodable
    (t (hpack-encode-headers
        (make-hpack-context)
        (append '((":method" . "POST") (":scheme" . "https") (":path" . "/"))
                (when (rng-bool rng)
                  (list (cons "content-length" (princ-to-string (rng-int rng 0 3000)))))
                (gen-header-list rng 4))))))

(defun gen-settings-payload (rng)
  (let ((entries (loop repeat (rng-int rng 0 6)
                       collect (cons (rng-choose rng '(1 2 3 4 5 6 #x77))
                                     (rng-choose rng (list 0 1 100 16384 65535
                                                           +max-window-size+
                                                           (1+ +max-window-size+)
                                                           (rng-int rng 0 #xFFFFFF)))))))
    (let ((v (make-array (* 6 (length entries)) :element-type '(unsigned-byte 8))))
      (loop for (id . value) in entries
            for i from 0 by 6
            do (setf (aref v i) (ldb (byte 8 8) id)
                     (aref v (+ i 1)) (ldb (byte 8 0) id))
               (replace v (u32-octets value) :start1 (+ i 2)))
      (if (zerop (rng-int rng 0 15))
          (subseq v 0 (max 0 (1- (length v)))) ; a bad length
          v))))

(defun gen-valid-settings-payload (rng)
  (let ((entries (loop repeat (rng-int rng 0 4)
                       collect (rng-choose rng (list (cons 1 (rng-choose rng '(0 100 4096)))
                                                     (cons 2 0)
                                                     (cons 3 100)
                                                     (cons 4 (rng-choose rng (list 0 1000 65535
                                                                                   1000000)))
                                                     (cons 5 (rng-choose rng '(16384 32768)))
                                                     (cons 6 1000)
                                                     (cons #x77 (rng-int rng 0 1000)))))))
    (let ((v (make-array (* 6 (length entries)) :element-type '(unsigned-byte 8))))
      (loop for (id . value) in entries
            for i from 0 by 6
            do (setf (aref v i) (ldb (byte 8 8) id)
                     (aref v (+ i 1)) (ldb (byte 8 0) id))
               (replace v (u32-octets value) :start1 (+ i 2)))
      v)))

(defun gen-one-frame (rng)
  "A frame spec (type flags stream-id payload): valid or not."
  (let ((sid (gen-stream-id rng)))
    (case (rng-int rng 0 9)
      ((0 1)
       (list +frame-headers+
             (logior (if (rng-bool rng) +flag-end-headers+ 0)
                     (if (rng-bool rng) 1 0)) ; END_STREAM
             sid (gen-header-block rng)))
      (2
       (list +frame-continuation+ (if (rng-bool rng) +flag-end-headers+ 0)
             sid (gen-octets rng (rng-int rng 0 8))))
      ((3 4)
       (let ((n (rng-choose rng (list 0 1 100 1000 16384 (rng-int rng 0 2000)))))
         (list +frame-data+ (if (rng-bool rng) 1 0) sid (gen-octets rng n 7))))
      (5
       (list 3 0 sid (if (zerop (rng-int rng 0 7))
                         (gen-octets rng 3)
                         (u32-octets (rng-int rng 0 13)))))
      ((6 7)
       (list +frame-window-update+ 0 (if (rng-bool rng) 0 sid)
             (u32-octets (rng-choose rng (list 0 1 1000 65535 +max-window-size+
                                               (rng-int rng 1 +max-window-size+))))))
      (t
       (list +frame-settings+ (if (zerop (rng-int rng 0 7)) +flag-ack+ 0)
             (if (zerop (rng-int rng 0 15)) 1 0)
             (gen-settings-payload rng))))))

(defun gen-header-run (rng sid block end-stream)
  "HEADERS carrying BLOCK on SID, split over CONTINUATIONs about a third of
   the time. A run is sometimes padded with empty frames past the count cap."
  (if (< (rng-int rng 0 2) 2)
      (list (list +frame-headers+ (logior +flag-end-headers+ (if end-stream 1 0))
                  sid block))
      (let* ((cut (rng-int rng 0 (length block)))
             (empties (rng-choose rng (list 0 0 1 10 62 63 64 (rng-int rng 0 150)))))
        (append (list (list +frame-headers+ (if end-stream 1 0) sid (subseq block 0 cut)))
                (loop repeat empties
                      collect (list +frame-continuation+ 0 sid (gen-octets rng 0)))
                (list (list +frame-continuation+ +flag-end-headers+ sid
                            (subseq block cut)))))))

(defun gen-frame-sequence (rng size)
  "Mostly well-formed traffic on a handful of streams, with invalid frames
   mixed in: requests (some split over CONTINUATION), trailers, DATA (some
   past the body cap, so we RST), RST_STREAM, WINDOW_UPDATE and SETTINGS."
  (let ((frames nil)
        (next-id 1)
        (used nil)
        (chaos (rng-choose rng '(0 2 5 20))))  ; percent of arbitrary frames
    (flet ((emit (list) (dolist (f list) (push f frames)))
           (some-id ()
             (if used (rng-choose rng used) 1)))
      ;; One request first, so frames on a used id are not all idle errors.
      (push (list +frame-headers+ +flag-end-headers+ 1
                  (hpack-encode-headers (make-hpack-context)
                                        '((":method" . "POST") (":path" . "/"))))
            frames)
      (setf next-id 3 used (list 1))
      (loop repeat (rng-int rng 1 (+ 10 (* 4 size)))
            do (if (< (rng-int rng 0 99) chaos)
                   (emit (list (gen-one-frame rng)))
                   (case (rng-int rng 0 9)
                     ((0 1)
                      (let ((sid next-id))
                        (incf next-id 2)
                        (push sid used)
                        (emit (gen-header-run rng sid (gen-header-block rng)
                                              (zerop (rng-int rng 0 3))))))
                     (2
                      (emit (gen-header-run rng (some-id)
                                            (if (rng-bool rng)
                                                (gen-header-block rng)
                                                (hpack-encode-headers
                                                 (make-hpack-context)
                                                 '(("x-trailer" . "t"))))
                                            (rng-int rng 0 5))))
                     ((3 4 5)
                      (emit (list (list +frame-data+ (if (zerop (rng-int rng 0 3)) 1 0)
                                        (some-id)
                                        (gen-octets rng (rng-choose rng '(0 1 100 1000 2999 4000
                                                                         16384))
                                                    7)))))
                     (6
                      (emit (list (list 3 0 (some-id) (u32-octets (rng-int rng 0 13))))))
                     (7
                      (emit (list (list +frame-window-update+ 0
                                        (if (rng-bool rng) 0 (some-id))
                                        (u32-octets
                                         (rng-choose rng (list 1 1000 65535
                                                               (rng-int rng 1 100000)
                                                               (if (zerop (rng-int rng 0 4))
                                                                   +max-window-size+
                                                                   1))))))))
                     (t
                      (emit (list (list +frame-settings+ 0 0
                                        (gen-valid-settings-payload rng))))))))
      (nreverse frames))))

(defun window-ok-p (n)
  (<= (- +max-window-size+) n +max-window-size+))

(defun connection-invariants-hold-p (specs)
  "Feed SPECS to a fresh connection and check invariants after every frame."
  (let* ((codes nil)
         (sent nil)
         (header-input 0)
         (run 0)                        ; CONTINUATIONs since the last HEADERS
         (conn (make-http2-connection
                :on-error (lambda (c d) (declare (ignore d)) (push c codes))))
         (woo.http2.connection:*http2-frame-sink* (lambda (f) (push f sent)))
         (woo.http2.connection:*max-request-body-size* 3000))
    (dolist (spec specs t)
      (destructuring-bind (type flags sid payload) spec
        (when (member type (list +frame-headers+ +frame-continuation+))
          (incf header-input (length payload)))
        (setf run (cond ((= type +frame-continuation+) (1+ run))
                        ((= type +frame-headers+) 0)
                        (t run)))
        (let ((goaway-before (http2-connection-goaway-sent conn))
              (sent-before (length sent)))
          ;; Nothing escapes: the handler inside reports a Lisp error as
          ;; INTERNAL_ERROR, which is also a failure here.
          (connection-process-frame conn (make-frame :type type :flags flags
                                                     :stream-id sid :payload payload))
          (unless (and
                   (not (member woo.http2.constants:+internal-error+ codes))
                   (window-ok-p (woo.http2.connection:http2-connection-window-size conn))
                   (window-ok-p (woo.http2.connection:http2-connection-remote-window-size conn))
                   (loop for stream being the hash-values
                           of (woo.http2.connection:http2-connection-streams conn)
                         always (and (window-ok-p (woo.http2.stream:http2-stream-window-size stream))
                                     (window-ok-p (woo.http2.stream:http2-stream-recv-window-size
                                                   stream))))
                   (<= (length (woo.http2.connection:http2-connection-remote-settings conn)) 6)
                   (<= (hash-table-count
                        (woo.http2.connection::http2-connection-closed-streams conn))
                       woo.http2.connection::*closed-stream-retention*)
                   ;; After GOAWAY nothing more is sent.
                   (or (not goaway-before) (= (length sent) sent-before))
                   (<= (count woo.http2.constants:+frame-goaway+ sent
                              :key #'frame-type)
                       1)
                   ;; A header block cannot be stretched past the frame cap.
                   (or (<= run woo.http2.connection:*max-continuation-frames*)
                       (http2-connection-goaway-sent conn))
                   ;; Header buffering stays linear in the header octets received.
                   (<= (woo.http2.connection::http2-connection-header-octets-copied conn)
                       (+ 64 (* 4 header-input))))
            (format t "~&invariant broken after ~S~%" (list type flags sid (length payload)))
            (return nil)))))))

(deftest prop-connection-frame-sequences
  (ok (check-property "http2-connection-random-frames"
                      #'connection-invariants-hold-p
                      #'gen-frame-sequence
                      :shrinker (lambda (specs) (shrink-list specs (constantly nil)))
                      :iters (* 5 (prop-iters)))
      "random HEADERS/CONTINUATION/DATA/RST/WINDOW_UPDATE/SETTINGS keep the invariants"))
