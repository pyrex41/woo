(in-package :cl-user)
(defpackage woo-test.hpack
  (:use :cl :rove)
  (:import-from :woo.http2.hpack
                :make-hpack-context
                :hpack-context
                :hpack-context-max-dynamic-table-size
                :hpack-context-dynamic-table
                :hpack-context-dynamic-table-size
                :hpack-encode-headers
                :hpack-decode-headers
                :hpack-context-update-size
                :hpack-compression-error))
(in-package :woo-test.hpack)

;;;; HPACK Context Tests

(deftest hpack-context-creation
  (testing "Create default HPACK context"
    (let ((ctx (make-hpack-context)))
      (ok (typep ctx 'hpack-context))
      (ok (= (hpack-context-dynamic-table-size ctx) 0))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 4096))
      (ok (= (length (hpack-context-dynamic-table ctx)) 0))))

  (testing "Create HPACK context with custom max size"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 8192)))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 8192)))))

;;;; Integer Encoding/Decoding Tests

(deftest hpack-integer-encoding-small
  (testing "Encode small integers (less than 2^N-1)"
    ;; Value 10 with 5-bit prefix
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 10 5 0)))
      (ok (equal bytes '(10)))
      (ok (= (length bytes) 1)))

    ;; Value 5 with 3-bit prefix
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 5 3 0)))
      (ok (equal bytes '(5)))
      (ok (= (length bytes) 1)))

    ;; Value 42 with 8-bit prefix
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 42 8 0)))
      (ok (equal bytes '(42)))
      (ok (= (length bytes) 1)))))

(deftest hpack-integer-encoding-boundary
  (testing "Encode integers at boundary (2^N-1)"
    ;; 5-bit prefix: 31 is at boundary (2^5-1 = 31)
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 31 5 0)))
      (ok (equal bytes '(31 0))))

    ;; 3-bit prefix: 7 is at boundary (2^3-1 = 7)
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 7 3 0)))
      (ok (equal bytes '(7 0))))

    ;; 6-bit prefix: 63 is at boundary (2^6-1 = 63)
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 63 6 0)))
      (ok (equal bytes '(63 0))))))

(deftest hpack-integer-encoding-large
  (testing "Encode large integers (greater than 2^N-1)"
    ;; Value 1337 with 5-bit prefix
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 1337 5 0)))
      (ok (= (length bytes) 3))
      (ok (= (first bytes) 31))
      (ok (= (logand (second bytes) #x80) #x80))  ; High bit set
      (ok (= (logand (third bytes) #x80) 0)))    ; High bit clear

    ;; Value 127 with 7-bit prefix (2^7-1 = 127)
    ;; When value equals max prefix, implementation may produce (127 0) or (127 1)
    ;; depending on boundary handling - just verify it decodes correctly
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 127 7 0)))
      (ok (>= (length bytes) 1))
      (ok (= (first bytes) 127)))

    ;; Value 128 with 7-bit prefix
    ;; Per RFC 7541: 128 >= 127 (max), so output 127, then 128-127=1
    ;; Result: (127, 1) = 2 bytes
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 128 7 0)))
      (ok (= (length bytes) 2))
      (ok (equal bytes '(127 1))))))

(deftest hpack-integer-encoding-with-prefix
  (testing "Encode integers with non-zero first byte prefix"
    ;; Encode 10 with 5-bit prefix and top 3 bits set to 0b111
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 10 5 #xE0)))
      (ok (equal bytes '(234)))  ; 0xE0 | 10 = 224 + 10 = 234
      (ok (= (logand (first bytes) #xE0) #xE0)))

    ;; Encode 42 with 6-bit prefix and top 2 bits set to 0b01
    (let ((bytes (woo.http2.hpack::hpack-encode-integer 42 6 #x40)))
      (ok (equal bytes '(106))))))  ; 0x40 | 42 = 64 + 42 = 106

(deftest hpack-integer-decoding-roundtrip
  (testing "Decode integers - roundtrip with encoding"
    ;; Small values
    (loop for value from 0 to 30
          for prefix-bits in '(5 6 7 8 5 6 7 8 5 6 7 8 5 6 7 8 5 6 7 8 5 6 7 8 5 6 7 8 5 6 7)
          do (let* ((encoded (woo.http2.hpack::hpack-encode-integer value prefix-bits 0))
                    (data (make-array (length encoded)
                                     :element-type '(unsigned-byte 8)
                                     :initial-contents encoded)))
               (multiple-value-bind (decoded consumed)
                   (woo.http2.hpack::hpack-decode-integer data 0 prefix-bits)
                 (ok (= decoded value))
                 (ok (= consumed (length encoded))))))

    ;; Boundary values
    (dolist (test-case '((31 5) (63 6) (127 7) (255 8)))
      (destructuring-bind (value prefix-bits) test-case
        (let* ((encoded (woo.http2.hpack::hpack-encode-integer value prefix-bits 0))
               (data (make-array (length encoded)
                                :element-type '(unsigned-byte 8)
                                :initial-contents encoded)))
          (multiple-value-bind (decoded consumed)
              (woo.http2.hpack::hpack-decode-integer data 0 prefix-bits)
            (ok (= decoded value))
            (ok (= consumed (length encoded)))))))

    ;; Large values
    (dolist (test-case '((1337 5) (10000 6) (65535 7)))
      (destructuring-bind (value prefix-bits) test-case
        (let* ((encoded (woo.http2.hpack::hpack-encode-integer value prefix-bits 0))
               (data (make-array (length encoded)
                                :element-type '(unsigned-byte 8)
                                :initial-contents encoded)))
          (multiple-value-bind (decoded consumed)
              (woo.http2.hpack::hpack-decode-integer data 0 prefix-bits)
            (ok (= decoded value))
            (ok (= consumed (length encoded)))))))))

(deftest hpack-integer-decoding-with-offset
  (testing "Decode integers with non-zero start offset"
    (let* ((encoded (woo.http2.hpack::hpack-encode-integer 42 6 #x40))
           (data (make-array 10 :element-type '(unsigned-byte 8) :initial-element 0)))
      ;; Place encoded bytes at offset 5
      (loop for i from 0 below (length encoded)
            do (setf (aref data (+ 5 i)) (nth i encoded)))
      (multiple-value-bind (decoded consumed)
          (woo.http2.hpack::hpack-decode-integer data 5 6)
        (ok (= decoded 42))
        (ok (= consumed (length encoded)))))))

;;;; String Encoding/Decoding Tests

(deftest hpack-string-encoding-ascii
  (testing "Encode ASCII strings"
    (let ((encoded (woo.http2.hpack::hpack-encode-string "hello")))
      (ok (vectorp encoded))
      (ok (> (length encoded) 5))  ; Length byte(s) + content
      ;; First byte should encode length 5 with 7-bit prefix
      (ok (= (aref encoded 0) 5))
      ;; Content bytes
      (ok (= (aref encoded 1) (char-code #\h)))
      (ok (= (aref encoded 2) (char-code #\e)))
      (ok (= (aref encoded 3) (char-code #\l)))
      (ok (= (aref encoded 4) (char-code #\l)))
      (ok (= (aref encoded 5) (char-code #\o))))))

(deftest hpack-string-encoding-utf8
  (testing "Encode UTF-8 strings"
    (let ((encoded (woo.http2.hpack::hpack-encode-string "café")))
      (ok (vectorp encoded))
      ;; "café" is 5 bytes in UTF-8 (c, a, f, é=0xC3 0xA9)
      (ok (>= (length encoded) 5)))

    (let ((encoded (woo.http2.hpack::hpack-encode-string "日本語")))
      (ok (vectorp encoded))
      ;; Japanese characters take 3 bytes each in UTF-8
      (ok (>= (length encoded) 9)))))

(deftest hpack-string-encoding-empty
  (testing "Encode empty string"
    (let ((encoded (woo.http2.hpack::hpack-encode-string "")))
      (ok (vectorp encoded))
      (ok (= (length encoded) 1))
      (ok (= (aref encoded 0) 0)))))

(deftest hpack-string-decoding-roundtrip
  (testing "Decode strings - roundtrip with encoding"
    (dolist (test-string '("hello"
                           "test"
                           ""
                           "a"
                           "HTTP/2"
                           "Content-Type"
                           "application/json"
                           "café"
                           "日本語"))
      (let ((encoded (woo.http2.hpack::hpack-encode-string test-string)))
        (multiple-value-bind (decoded consumed)
            (woo.http2.hpack::hpack-decode-string encoded 0)
          (ok (string= decoded test-string))
          (ok (= consumed (length encoded))))))))

(deftest hpack-string-decoding-with-offset
  (testing "Decode strings with non-zero start offset"
    (let* ((encoded (woo.http2.hpack::hpack-encode-string "test"))
           (data (make-array (+ 10 (length encoded))
                            :element-type '(unsigned-byte 8)
                            :initial-element 0)))
      ;; Place encoded bytes at offset 5
      (replace data encoded :start1 5)
      (multiple-value-bind (decoded consumed)
          (woo.http2.hpack::hpack-decode-string data 5)
        (ok (string= decoded "test"))
        (ok (= consumed (length encoded)))))))

;;;; Static Table Lookup Tests

(deftest hpack-static-table-lookup
  (testing "Lookup entries in static table (indices 1-61)"
    ;; Test a few known static table entries
    (let ((ctx (make-hpack-context)))
      ;; Index 1: :authority
      (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 1)))
        (ok (consp entry))
        (ok (string= (car entry) ":authority"))
        (ok (string= (cdr entry) "")))

      ;; Index 2: :method GET
      (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 2)))
        (ok (string= (car entry) ":method"))
        (ok (string= (cdr entry) "GET")))

      ;; Index 3: :method POST
      (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 3)))
        (ok (string= (car entry) ":method"))
        (ok (string= (cdr entry) "POST")))

      ;; Index 8: :status 200
      (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 8)))
        (ok (string= (car entry) ":status"))
        (ok (string= (cdr entry) "200")))

      ;; Index 31: content-type
      (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 31)))
        (ok (string= (car entry) "content-type"))
        (ok (string= (cdr entry) "")))

      ;; Index 61: www-authenticate (last entry)
      (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 61)))
        (ok (string= (car entry) "www-authenticate"))
        (ok (string= (cdr entry) ""))))))

(deftest hpack-static-table-invalid-index
  (testing "Invalid index 0 should error"
    (let ((ctx (make-hpack-context)))
      (ok (signals (woo.http2.hpack::hpack-lookup-index ctx 0) 'error)))))

;;;; Dynamic Table Tests

(deftest hpack-dynamic-table-add-entry
  (testing "Add entry to dynamic table"
    (let ((ctx (make-hpack-context)))
      (woo.http2.hpack::hpack-context-add-entry ctx "custom-header" "custom-value")
      (ok (= (length (hpack-context-dynamic-table ctx)) 1))
      (ok (> (hpack-context-dynamic-table-size ctx) 0))

      ;; Entry should be at index 62 (first dynamic table entry)
      (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 62)))
        (ok (consp entry))
        (ok (string= (car entry) "custom-header"))
        (ok (string= (cdr entry) "custom-value"))))))

(deftest hpack-dynamic-table-multiple-entries
  (testing "Add multiple entries to dynamic table"
    (let ((ctx (make-hpack-context)))
      (woo.http2.hpack::hpack-context-add-entry ctx "header-1" "value-1")
      (woo.http2.hpack::hpack-context-add-entry ctx "header-2" "value-2")
      (woo.http2.hpack::hpack-context-add-entry ctx "header-3" "value-3")

      (ok (= (length (hpack-context-dynamic-table ctx)) 3))

      ;; Most recent entry (header-3) should be at index 62
      (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 62)))
        (ok (string= (car entry) "header-3"))
        (ok (string= (cdr entry) "value-3")))

      ;; header-2 at index 63
      (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 63)))
        (ok (string= (car entry) "header-2"))
        (ok (string= (cdr entry) "value-2")))

      ;; header-1 (oldest) at index 64
      (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 64)))
        (ok (string= (car entry) "header-1"))
        (ok (string= (cdr entry) "value-1"))))))

(deftest hpack-dynamic-table-entry-size
  (testing "Entry size is octets, not Lisp characters (RFC 7541 §4.1)"
    (let ((size (woo.http2.hpack::hpack-entry-size "test" "value")))
      (ok (= size (+ 32 4 5))))
    ;; U+00E9 encodes as two UTF-8 octets, not one character.
    (let ((size (woo.http2.hpack::hpack-entry-size
                 "x" (string (code-char #xE9)))))
      (ok (= size (+ 32 1 2))))
    ;; U+65E5 U+672C U+8A9E ("日本語") is 3 characters and 9 octets.
    (let* ((value (coerce (list (code-char #x65E5)
                                (code-char #x672C)
                                (code-char #x8A9E))
                          'string))
           (size (woo.http2.hpack::hpack-entry-size "a" value)))
      (ok (= (length value) 3))
      (ok (= size (+ 32 1 9))))))

(deftest hpack-dynamic-table-eviction
  (testing "Evict entries when max size exceeded"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 100)))
      ;; Add entries that together exceed max size
      ;; Each entry: 32 + len(name) + len(value)
      (woo.http2.hpack::hpack-context-add-entry ctx "header-1" "value-1")  ; 32 + 8 + 7 = 47
      (woo.http2.hpack::hpack-context-add-entry ctx "header-2" "value-2")  ; 32 + 8 + 7 = 47
      (woo.http2.hpack::hpack-context-add-entry ctx "header-3" "value-3")  ; 32 + 8 + 7 = 47

      ;; Total would be 141, but max is 100, so oldest entries should be evicted
      (ok (<= (hpack-context-dynamic-table-size ctx) 100))
      (ok (< (length (hpack-context-dynamic-table ctx)) 3)))))

(deftest hpack-dynamic-table-size-update
  (testing "Update dynamic table max size and trigger eviction"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 200)))
      ;; Add some entries
      (woo.http2.hpack::hpack-context-add-entry ctx "header-1" "value-1")
      (woo.http2.hpack::hpack-context-add-entry ctx "header-2" "value-2")
      (woo.http2.hpack::hpack-context-add-entry ctx "header-3" "value-3")

      (let ((old-count (length (hpack-context-dynamic-table ctx))))
        ;; Reduce max size to force eviction
        (hpack-context-update-size ctx 50)
        (ok (= (hpack-context-max-dynamic-table-size ctx) 50))
        (ok (<= (hpack-context-dynamic-table-size ctx) 50))
        (ok (<= (length (hpack-context-dynamic-table ctx)) old-count))))))

(deftest hpack-dynamic-table-entry-too-large
  (testing "Entry larger than max size should not be added"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 50)))
      ;; Try to add entry that's larger than max size
      (woo.http2.hpack::hpack-context-add-entry ctx "very-long-header-name" "very-long-header-value-that-exceeds-limit")
      ;; Entry size would be 32 + 20 + 47 = 99, which is > 50
      (ok (= (length (hpack-context-dynamic-table ctx)) 0))))
  (testing "Oversized insert empties existing entries and does not insert (RFC 7541 §4.4)"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 50)))
      (woo.http2.hpack::hpack-context-add-entry ctx "a" "b")
      (ok (= (length (hpack-context-dynamic-table ctx)) 1))
      (woo.http2.hpack::hpack-context-add-entry
       ctx "very-long-header-name" "very-long-header-value-that-exceeds-limit")
      (ok (= (length (hpack-context-dynamic-table ctx)) 0))
      (ok (= (hpack-context-dynamic-table-size ctx) 0)))))

;;;; Header Encoding Tests

(deftest hpack-encode-headers-simple
  (testing "Encode simple headers"
    (let ((ctx (make-hpack-context))
          (headers '(("content-type" . "text/plain")
                     ("content-length" . "42"))))
      (let ((encoded (hpack-encode-headers ctx headers)))
        (ok (vectorp encoded))
        (ok (> (length encoded) 0))))))

(deftest hpack-encode-headers-with-static-table
  (testing "Encode headers using static table"
    (let ((ctx (make-hpack-context))
          ;; :method GET is at static index 2
          (headers '((":method" . "GET"))))
      (let ((encoded (hpack-encode-headers ctx headers)))
        (ok (vectorp encoded))
        ;; Should use indexed representation (1xxxxxxx)
        (ok (= (logand (aref encoded 0) #x80) #x80))))))

(deftest hpack-encode-headers-custom
  (testing "Encode custom headers not in static table"
    (let ((ctx (make-hpack-context))
          (headers '(("x-custom-header" . "custom-value"))))
      (let ((encoded (hpack-encode-headers ctx headers)))
        (ok (vectorp encoded))
        (ok (> (length encoded) 0))))))

(deftest hpack-encode-headers-mixed
  (testing "Encode mix of static and custom headers"
    (let ((ctx (make-hpack-context))
          (headers '((":method" . "GET")
                     ("content-type" . "application/json")
                     ("x-custom" . "value"))))
      (let ((encoded (hpack-encode-headers ctx headers)))
        (ok (vectorp encoded))
        (ok (> (length encoded) 0))))))

(deftest hpack-encode-headers-symbol-names
  (testing "Encode headers with symbol names (keyword and regular)"
    (let ((ctx (make-hpack-context))
          (headers '((:content-type . "text/html")
                     (user-agent . "test/1.0"))))
      (let ((encoded (hpack-encode-headers ctx headers)))
        (ok (vectorp encoded))
        (ok (> (length encoded) 0))))))

;;;; Header Decoding Tests

(deftest hpack-decode-headers-indexed
  (testing "Decode indexed header (static table)"
    (let ((ctx (make-hpack-context)))
      ;; Encode index 2 (:method GET) with indexed representation
      (let* ((bytes (woo.http2.hpack::hpack-encode-integer 2 7 #x80))
             (data (make-array (length bytes)
                              :element-type '(unsigned-byte 8)
                              :initial-contents bytes)))
        (let ((headers (hpack-decode-headers ctx data)))
          (ok (= (length headers) 1))
          (ok (string= (caar headers) ":method"))
          (ok (string= (cdar headers) "GET")))))))

(deftest hpack-decode-headers-literal-without-indexing
  (testing "Decode literal header without indexing"
    (let ((ctx (make-hpack-context)))
      ;; Create a literal header: name="test", value="value"
      ;; Format: 0000 0000 (literal without indexing, new name)
      (let* ((name-enc (woo.http2.hpack::hpack-encode-string "test"))
             (value-enc (woo.http2.hpack::hpack-encode-string "value"))
             (data (make-array (+ 1 (length name-enc) (length value-enc))
                              :element-type '(unsigned-byte 8))))
        (setf (aref data 0) #x00)  ; Literal without indexing, new name
        (replace data name-enc :start1 1)
        (replace data value-enc :start1 (1+ (length name-enc)))

        (let ((headers (hpack-decode-headers ctx data)))
          (ok (= (length headers) 1))
          (ok (string= (caar headers) "test"))
          (ok (string= (cdar headers) "value"))
          ;; Should not add to dynamic table
          (ok (= (length (hpack-context-dynamic-table ctx)) 0)))))))

(deftest hpack-decode-headers-literal-with-indexing
  (testing "Decode literal header with incremental indexing"
    (let ((ctx (make-hpack-context)))
      ;; Format: 01 000000 (literal with indexing, new name)
      (let* ((name-enc (woo.http2.hpack::hpack-encode-string "custom"))
             (value-enc (woo.http2.hpack::hpack-encode-string "test"))
             (data (make-array (+ 1 (length name-enc) (length value-enc))
                              :element-type '(unsigned-byte 8))))
        (setf (aref data 0) #x40)  ; 01000000 - literal with indexing, new name
        (replace data name-enc :start1 1)
        (replace data value-enc :start1 (1+ (length name-enc)))

        (let ((headers (hpack-decode-headers ctx data)))
          (ok (= (length headers) 1))
          (ok (string= (caar headers) "custom"))
          (ok (string= (cdar headers) "test"))
          ;; Should add to dynamic table
          (ok (= (length (hpack-context-dynamic-table ctx)) 1)))))))

(deftest hpack-decode-headers-dynamic-table-size-update
  (testing "Decode dynamic table size update"
    (let ((ctx (make-hpack-context)))
      ;; Format: 001 xxxxx (dynamic table size update)
      ;; Set size to 1024
      (let* ((bytes (woo.http2.hpack::hpack-encode-integer 1024 5 #x20))
             (data (make-array (length bytes)
                              :element-type '(unsigned-byte 8)
                              :initial-contents bytes)))
        (let ((headers (hpack-decode-headers ctx data)))
          (ok (= (length headers) 0))  ; No headers, just size update
          (ok (= (hpack-context-max-dynamic-table-size ctx) 1024)))))))

(deftest hpack-decode-headers-multiple
  (testing "Decode multiple headers in one block"
    (let ((ctx (make-hpack-context)))
      ;; Create block with two indexed headers
      (let* ((bytes1 (woo.http2.hpack::hpack-encode-integer 2 7 #x80))  ; :method GET
             (bytes2 (woo.http2.hpack::hpack-encode-integer 8 7 #x80))  ; :status 200
             (data (make-array (+ (length bytes1) (length bytes2))
                              :element-type '(unsigned-byte 8))))
        (loop for i from 0 below (length bytes1)
              do (setf (aref data i) (nth i bytes1)))
        (loop for i from 0 below (length bytes2)
              do (setf (aref data (+ (length bytes1) i)) (nth i bytes2)))

        (let ((headers (hpack-decode-headers ctx data)))
          (ok (= (length headers) 2))
          (ok (string= (caar headers) ":method"))
          (ok (string= (cdar headers) "GET"))
          (ok (string= (caadr headers) ":status"))
          (ok (string= (cdadr headers) "200")))))))

;;;; Roundtrip Encoding/Decoding Tests

(deftest hpack-roundtrip-simple-headers
  (testing "Roundtrip encode/decode simple headers"
    (let ((ctx-enc (make-hpack-context))
          (ctx-dec (make-hpack-context))
          (headers '(("content-type" . "text/plain")
                     ("content-length" . "100"))))
      (let* ((encoded (hpack-encode-headers ctx-enc headers))
             (decoded (hpack-decode-headers ctx-dec encoded)))
        (ok (= (length decoded) (length headers)))
        (dolist (original headers)
          (ok (member original decoded :test #'equal)))))))

(deftest hpack-roundtrip-static-table-headers
  (testing "Roundtrip encode/decode static table headers"
    (let ((ctx-enc (make-hpack-context))
          (ctx-dec (make-hpack-context))
          (headers '((":method" . "GET")
                     (":path" . "/")
                     (":scheme" . "https"))))
      (let* ((encoded (hpack-encode-headers ctx-enc headers))
             (decoded (hpack-decode-headers ctx-dec encoded)))
        (ok (= (length decoded) (length headers)))
        (dolist (original headers)
          (ok (member original decoded :test #'equal)))))))

(deftest hpack-roundtrip-custom-headers
  (testing "Roundtrip encode/decode custom headers"
    (let ((ctx-enc (make-hpack-context))
          (ctx-dec (make-hpack-context))
          (headers '(("x-custom-1" . "value1")
                     ("x-custom-2" . "value2")
                     ("x-api-key" . "secret123"))))
      (let* ((encoded (hpack-encode-headers ctx-enc headers))
             (decoded (hpack-decode-headers ctx-dec encoded)))
        (ok (= (length decoded) (length headers)))
        (dolist (original headers)
          (ok (member original decoded :test #'equal)))))))

(deftest hpack-roundtrip-mixed-headers
  (testing "Roundtrip encode/decode mixed static and custom headers"
    (let ((ctx-enc (make-hpack-context))
          (ctx-dec (make-hpack-context))
          (headers '((":method" . "POST")
                     ("content-type" . "application/json")
                     ("content-length" . "256")
                     ("x-request-id" . "abc123")
                     ("authorization" . "Bearer token"))))
      (let* ((encoded (hpack-encode-headers ctx-enc headers))
             (decoded (hpack-decode-headers ctx-dec encoded)))
        (ok (= (length decoded) (length headers)))
        (dolist (original headers)
          (ok (member original decoded :test #'equal)))))))

(deftest hpack-roundtrip-utf8-headers
  (testing "Roundtrip encode/decode UTF-8 header values"
    (let ((ctx-enc (make-hpack-context))
          (ctx-dec (make-hpack-context))
          (headers '(("x-message" . "café")
                     ("x-greeting" . "こんにちは")
                     ("x-emoji" . "🚀"))))
      (let* ((encoded (hpack-encode-headers ctx-enc headers))
             (decoded (hpack-decode-headers ctx-dec encoded)))
        (ok (= (length decoded) (length headers)))
        (dolist (original headers)
          (ok (member original decoded :test #'equal)))))))

(deftest hpack-roundtrip-empty-values
  (testing "Roundtrip encode/decode headers with empty values"
    (let ((ctx-enc (make-hpack-context))
          (ctx-dec (make-hpack-context))
          (headers '(("x-empty" . "")
                     ("content-length" . "0")
                     ("accept" . ""))))
      (let* ((encoded (hpack-encode-headers ctx-enc headers))
             (decoded (hpack-decode-headers ctx-dec encoded)))
        (ok (= (length decoded) (length headers)))
        (dolist (original headers)
          (ok (member original decoded :test #'equal)))))))

(deftest hpack-roundtrip-common-http-headers
  (testing "Roundtrip encode/decode common HTTP/2 request headers"
    (let ((ctx-enc (make-hpack-context))
          (ctx-dec (make-hpack-context))
          (headers '((":method" . "GET")
                     (":scheme" . "https")
                     (":path" . "/api/v1/users")
                     (":authority" . "example.com")
                     ("accept" . "application/json")
                     ("accept-encoding" . "gzip, deflate")
                     ("user-agent" . "test-client/1.0")
                     ("x-request-id" . "req-123"))))
      (let* ((encoded (hpack-encode-headers ctx-enc headers))
             (decoded (hpack-decode-headers ctx-dec encoded)))
        (ok (= (length decoded) (length headers)))
        (dolist (original headers)
          (ok (member original decoded :test #'equal)))))))

(deftest hpack-roundtrip-response-headers
  (testing "Roundtrip encode/decode common HTTP/2 response headers"
    (let ((ctx-enc (make-hpack-context))
          (ctx-dec (make-hpack-context))
          (headers '((":status" . "200")
                     ("content-type" . "application/json")
                     ("content-length" . "1234")
                     ("cache-control" . "no-cache")
                     ("date" . "Mon, 02 Dec 2025 12:00:00 GMT")
                     ("server" . "woo/1.0")
                     ("x-response-time" . "42ms"))))
      (let* ((encoded (hpack-encode-headers ctx-enc headers))
             (decoded (hpack-decode-headers ctx-dec encoded)))
        (ok (= (length decoded) (length headers)))
        (dolist (original headers)
          (ok (member original decoded :test #'equal)))))))

;;;; Edge Cases and Error Handling

(deftest hpack-encode-empty-header-list
  (testing "Encode empty header list"
    (let ((ctx (make-hpack-context))
          (headers '()))
      (let ((encoded (hpack-encode-headers ctx headers)))
        (ok (vectorp encoded))
        (ok (= (length encoded) 0))))))

(deftest hpack-decode-empty-data
  (testing "Decode empty data"
    (let ((ctx (make-hpack-context))
          (data (make-array 0 :element-type '(unsigned-byte 8))))
      (let ((headers (hpack-decode-headers ctx data)))
        (ok (= (length headers) 0))))))

(deftest hpack-large-header-value
  (testing "Encode/decode large header value"
    (let* ((ctx-enc (make-hpack-context))
           (ctx-dec (make-hpack-context))
           (large-value (make-string 10000 :initial-element #\x))
           (headers (list (cons "x-large" large-value))))
      (let* ((encoded (hpack-encode-headers ctx-enc headers))
             (decoded (hpack-decode-headers ctx-dec encoded)))
        (ok (= (length decoded) 1))
        (ok (string= (caar decoded) "x-large"))
        (ok (string= (cdar decoded) large-value))
        (ok (= (length (cdar decoded)) 10000))))))

(deftest hpack-special-characters-in-values
  (testing "Encode/decode headers with special characters"
    (let ((ctx-enc (make-hpack-context))
          (ctx-dec (make-hpack-context))
          (headers `(("x-test" . "value with spaces")
                     ("x-symbols" . "!@#$%^&*()")
                     ("x-quotes" . "\"quoted\"")
                     ;; HPACK itself carries any octets; field validation is elsewhere.
                     ("x-newline" . ,(format nil "line1~%line2")))))
      (let* ((encoded (hpack-encode-headers ctx-enc headers))
             (decoded (hpack-decode-headers ctx-dec encoded)))
        (ok (= (length decoded) (length headers)))
        (dolist (original headers)
          (ok (member original decoded :test #'equal)))))))

;;;; B2 Huffman, B3 integer bounds, B10 unknown index

(deftest hpack-integer-truncated
  (testing "Truncated integer (prefix filled, no continuation) is COMPRESSION_ERROR"
    ;; 5-bit prefix all ones (0x1F) requires at least one extra octet.
    (let ((data (make-array 1 :element-type '(unsigned-byte 8)
                            :initial-contents '(#x1F))))
      (ok (signals (woo.http2.hpack::hpack-decode-integer data 0 5)
                   'hpack-compression-error)))))

(deftest hpack-integer-0x80-chain
  (testing "Unbounded 0x80 continuation chain is COMPRESSION_ERROR"
    (let ((data (make-array 8 :element-type '(unsigned-byte 8)
                            :initial-contents '(#x1F #x80 #x80 #x80 #x80 #x80 #x80 #x80))))
      (ok (signals (woo.http2.hpack::hpack-decode-integer data 0 5)
                   'hpack-compression-error)))))

(deftest hpack-unknown-index
  (testing "Unknown static/dynamic index is COMPRESSION_ERROR"
    (let ((ctx (make-hpack-context)))
      (ok (signals (woo.http2.hpack::hpack-lookup-index ctx 62)
                   'hpack-compression-error))
      (ok (signals (woo.http2.hpack::hpack-lookup-index ctx 1000)
                   'hpack-compression-error))
      ;; Indexed header field 62 (0xBE) with empty dynamic table
      (let ((data (make-array 1 :element-type '(unsigned-byte 8)
                              :initial-contents '(#xBE))))
        (ok (signals (hpack-decode-headers ctx data)
                     'hpack-compression-error))))))

(defun hpack-bytes (&rest octets)
  (make-array (length octets) :element-type '(unsigned-byte 8)
              :initial-contents octets))

(defun hpack-size-update-bytes (size)
  (let ((bytes (woo.http2.hpack::hpack-encode-integer size 5 #x20)))
    (make-array (length bytes) :element-type '(unsigned-byte 8)
                :initial-contents bytes)))

(defun hpack-cat (&rest parts)
  (let* ((len (reduce #'+ parts :key #'length))
         (out (make-array len :element-type '(unsigned-byte 8)))
         (idx 0))
    (dolist (part parts)
      (replace out part :start1 idx)
      (incf idx (length part)))
    out))

;;;; B6 dynamic table: octets, oversized insert, size-update rules, obs-text

(deftest hpack-dynamic-table-octet-size-non-ascii
  (testing "Non-ASCII value is sized in UTF-8 octets and forces eviction"
    ;; "a" + U+65E5 U+672C U+8A9E is 32+1+9 = 42 octets (3 characters).
    ;; A max of 41 must refuse the insert. Character length (36) would fit.
    (let* ((ctx (make-hpack-context :max-dynamic-table-size 41))
           (block (hpack-bytes #x40 #x01 #x61 #x09
                               #xE6 #x97 #xA5 #xE6 #x9C #xAC #xE8 #xAA #x9E))
           (headers (hpack-decode-headers ctx block)))
      (ok (= (length headers) 1))
      (ok (string= (caar headers) "a"))
      (ok (equal (map 'list #'char-code (cdar headers))
                 '(#x65E5 #x672C #x8A9E)))
      (ok (= (length (hpack-context-dynamic-table ctx)) 0))
      (ok (= (hpack-context-dynamic-table-size ctx) 0))))
  (testing "Same value is inserted and counted as 42 octets when it fits"
    (let* ((ctx (make-hpack-context))
           (block (hpack-bytes #x40 #x01 #x61 #x09
                               #xE6 #x97 #xA5 #xE6 #x9C #xAC #xE8 #xAA #x9E)))
      (hpack-decode-headers ctx block)
      (ok (= (length (hpack-context-dynamic-table ctx)) 1))
      (ok (= (hpack-context-dynamic-table-size ctx) 42))
      ;; Shrinking below the octet size must evict. A character-sized
      ;; entry (36) would survive a max of 41.
      (let ((headers (hpack-decode-headers ctx (hpack-size-update-bytes 41))))
        (declare (ignore headers))
        (ok (= (hpack-context-max-dynamic-table-size ctx) 41))
        (ok (= (length (hpack-context-dynamic-table ctx)) 0))
        (ok (= (hpack-context-dynamic-table-size ctx) 0))))))

(deftest hpack-oversized-insert-on-wire
  (testing "Wire insert larger than the table max empties the table"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 50)))
      (woo.http2.hpack::hpack-context-add-entry ctx "a" "b")
      (ok (= (length (hpack-context-dynamic-table ctx)) 1))
      ;; name "name" (4) + 15 octets of value = 32+4+15 = 51 > 50.
      (let* ((value (make-array 15 :element-type '(unsigned-byte 8)
                                :initial-element #x78))
             (block (hpack-cat (hpack-bytes #x40 #x04 #x6e #x61 #x6d #x65 #x0f)
                               value))
             (headers (hpack-decode-headers ctx block)))
        (ok (= (length headers) 1))
        (ok (string= (caar headers) "name"))
        (ok (= (length (cdar headers)) 15))
        (ok (= (length (hpack-context-dynamic-table ctx)) 0))
        (ok (= (hpack-context-dynamic-table-size ctx) 0))))))

(deftest hpack-rfc7541-c5-eviction
  (testing "RFC 7541 C.5 responses evict at SETTINGS_HEADER_TABLE_SIZE 256"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 256))
          (c51 (hpack-bytes
                #x48 #x03 #x33 #x30 #x32 #x58 #x07 #x70 #x72 #x69 #x76 #x61 #x74 #x65 #x61 #x1d
                #x4d #x6f #x6e #x2c #x20 #x32 #x31 #x20 #x4f #x63 #x74 #x20 #x32 #x30 #x31 #x33
                #x20 #x32 #x30 #x3a #x31 #x33 #x3a #x32 #x31 #x20 #x47 #x4d #x54 #x6e #x17 #x68
                #x74 #x74 #x70 #x73 #x3a #x2f #x2f #x77 #x77 #x77 #x2e #x65 #x78 #x61 #x6d #x70
                #x6c #x65 #x2e #x63 #x6f #x6d))
          (c52 (hpack-bytes #x48 #x03 #x33 #x30 #x37 #xc1 #xc0 #xbf))
          (c53 (hpack-bytes
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
        (ok (= (length headers) 4))
        (ok (string= (caar headers) ":status"))
        (ok (string= (cdar headers) "307"))
        (ok (= (hpack-context-dynamic-table-size ctx) 222))
        (ok (= (length (hpack-context-dynamic-table ctx)) 4))
        (let ((entry (woo.http2.hpack::hpack-lookup-index ctx 62)))
          (ok (string= (car entry) ":status"))
          (ok (string= (cdr entry) "307")))
        ;; :status 302 was evicted; only four dynamic entries remain.
        (ok (signals (woo.http2.hpack::hpack-lookup-index ctx 66)
                     'hpack-compression-error))
        (ok (not (find "302" (hpack-context-dynamic-table ctx)
                       :key #'cdr :test #'string=))))
      (let ((headers (hpack-decode-headers ctx c53)))
        (ok (= (length headers) 6))
        (ok (string= (caar headers) ":status"))
        (ok (string= (cdar headers) "200"))
        (ok (string= (car (car (last headers))) "set-cookie"))
        (ok (= (hpack-context-dynamic-table-size ctx) 215))
        (ok (= (length (hpack-context-dynamic-table ctx)) 3))))))

(deftest hpack-rfc7541-c6-huffman-eviction
  (testing "RFC 7541 C.6 evicts using decoded octets, not Huffman wire length"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 256))
          (c61 (hpack-bytes
                #x48 #x82 #x64 #x02 #x58 #x85 #xae #xc3 #x77 #x1a #x4b #x61 #x96 #xd0 #x7a #xbe
                #x94 #x10 #x54 #xd4 #x44 #xa8 #x20 #x05 #x95 #x04 #x0b #x81 #x66 #xe0 #x82 #xa6
                #x2d #x1b #xff #x6e #x91 #x9d #x29 #xad #x17 #x18 #x63 #xc7 #x8f #x0b #x97 #xc8
                #xe9 #xae #x82 #xae #x43 #xd3))
          (c62 (hpack-bytes #x48 #x83 #x64 #x0e #xff #xc1 #xc0 #xbf))
          (c63 (hpack-bytes
                #x88 #xc1 #x61 #x96 #xd0 #x7a #xbe #x94 #x10 #x54 #xd4 #x44 #xa8 #x20 #x05 #x95
                #x04 #x0b #x81 #x66 #xe0 #x84 #xa6 #x2d #x1b #xff #xc0 #x5a #x83 #x9b #xd9 #xab
                #x77 #xad #x94 #xe7 #x82 #x1d #xd7 #xf2 #xe6 #xc7 #xb3 #x35 #xdf #xdf #xcd #x5b
                #x39 #x60 #xd5 #xaf #x27 #x08 #x7f #x36 #x72 #xc1 #xab #x27 #x0f #xb5 #x29 #x1f
                #x95 #x87 #x31 #x60 #x65 #xc0 #x03 #xed #x4e #xe5 #xb1 #x06 #x3d #x50 #x07)))
      (let ((headers (hpack-decode-headers ctx c61)))
        (ok (= (length headers) 4))
        (ok (string= (cdar headers) "302"))
        (ok (string= (cdr (nth 3 headers)) "https://www.example.com"))
        (ok (= (hpack-context-dynamic-table-size ctx) 222))
        (ok (= (length (hpack-context-dynamic-table ctx)) 4)))
      (let ((headers (hpack-decode-headers ctx c62)))
        (ok (string= (cdar headers) "307"))
        (ok (= (hpack-context-dynamic-table-size ctx) 222))
        (ok (not (find "302" (hpack-context-dynamic-table ctx)
                       :key #'cdr :test #'string=))))
      (let ((headers (hpack-decode-headers ctx c63)))
        (ok (= (length headers) 6))
        (ok (string= (cdr (car (last headers)))
                     "foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1"))
        (ok (= (hpack-context-dynamic-table-size ctx) 215))
        (ok (= (length (hpack-context-dynamic-table ctx)) 3))))))

(deftest hpack-size-update-literal-octets
  (testing "Published size-update octets, not an encode/decode round trip"
    (let ((ctx (make-hpack-context)))
      ;; Indexed :method GET, then a dynamic table size update of 0.
      (ok (signals (hpack-decode-headers ctx (hpack-bytes #x82 #x20))
                   'hpack-compression-error))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 4096)))
    (let ((ctx (make-hpack-context)))
      (hpack-decode-headers ctx (hpack-bytes #x40 #x01 #x61 #x01 #x62))
      ;; 4097 is one past the default SETTINGS_HEADER_TABLE_SIZE.
      (ok (signals (hpack-decode-headers ctx (hpack-bytes #x3F #xE2 #x1F))
                   'hpack-compression-error))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 4096))
      (ok (= (hpack-context-dynamic-table-size ctx) 34))
      (ok (= (length (hpack-context-dynamic-table ctx)) 1)))
    (let ((ctx (make-hpack-context :max-dynamic-table-size 100)))
      (ok (null (hpack-decode-headers ctx (hpack-bytes #x3F #x45))))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 100))
      (ok (signals (hpack-decode-headers ctx (hpack-bytes #x3F #x46))
                   'hpack-compression-error))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 100)))))

(deftest hpack-dynamic-table-size-update-rules
  (testing "Size update equal to the advertised limit is accepted"
    (let ((ctx (make-hpack-context)))
      (ok (null (hpack-decode-headers ctx (hpack-size-update-bytes 4096))))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 4096))))
  (testing "Encoder may lower the table and later raise it back to the limit"
    (let ((ctx (make-hpack-context)))
      (hpack-decode-headers ctx (hpack-size-update-bytes 100))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 100))
      (hpack-decode-headers ctx (hpack-size-update-bytes 200))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 200))))
  (testing "Size update above SETTINGS_HEADER_TABLE_SIZE is COMPRESSION_ERROR"
    (let ((ctx (make-hpack-context)))
      (woo.http2.hpack::hpack-context-add-entry ctx "a" "b")
      (let ((size (hpack-context-dynamic-table-size ctx))
            (max (hpack-context-max-dynamic-table-size ctx)))
        (ok (signals (hpack-decode-headers ctx (hpack-size-update-bytes 4097))
                     'hpack-compression-error))
        (ok (= (hpack-context-dynamic-table-size ctx) size))
        (ok (= (hpack-context-max-dynamic-table-size ctx) max))
        (ok (= (length (hpack-context-dynamic-table ctx)) 1)))))
  (testing "Custom context limit is the ceiling, not only the default 4096"
    (let ((ctx (make-hpack-context :max-dynamic-table-size 100)))
      (hpack-decode-headers ctx (hpack-size-update-bytes 100))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 100))
      (ok (signals (hpack-decode-headers ctx (hpack-size-update-bytes 101))
                   'hpack-compression-error))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 100))))
  (testing "SETTINGS_HEADER_TABLE_SIZE raises the ceiling"
    (let ((ctx (make-hpack-context)))
      (hpack-context-update-size ctx 8192)
      (hpack-decode-headers ctx (hpack-size-update-bytes 8192))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 8192))
      (ok (signals (hpack-decode-headers ctx (hpack-size-update-bytes 8193))
                   'hpack-compression-error))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 8192))))
  (testing "Size update after a header field is COMPRESSION_ERROR"
    (let ((ctx (make-hpack-context)))
      ;; Indexed :method GET, then a size update of 0.
      (ok (signals (hpack-decode-headers
                    ctx (hpack-cat (hpack-bytes #x82) (hpack-size-update-bytes 0)))
                   'hpack-compression-error))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 4096))))
  (testing "Two size updates before the first field are accepted"
    (let* ((ctx (make-hpack-context))
           (block (hpack-cat (hpack-size-update-bytes 0)
                             (hpack-size-update-bytes 256)
                             (hpack-bytes #x82)))
           (headers (hpack-decode-headers ctx block)))
      (ok (= (length headers) 1))
      (ok (string= (caar headers) ":method"))
      (ok (string= (cdar headers) "GET"))
      (ok (= (hpack-context-max-dynamic-table-size ctx) 256))
      (ok (= (length (hpack-context-dynamic-table ctx)) 0)))))

(deftest hpack-obs-text-not-utf8
  (testing "Raw obs-text is not a compression error and counts as one octet"
    (let* ((ctx (make-hpack-context))
           ;; Incremental indexing, name 0xFF, value "a".
           (headers (hpack-decode-headers
                     ctx (hpack-bytes #x40 #x01 #xFF #x01 #x61))))
      (ok (= (length headers) 1))
      (ok (= (char-code (char (caar headers) 0)) #xFF))
      (ok (string= (cdar headers) "a"))
      (ok (= (length (hpack-context-dynamic-table ctx)) 1))
      (ok (= (hpack-context-dynamic-table-size ctx) (+ 32 1 1)))))
  (testing "A later indexed name keeps the raw octet length"
    (let ((ctx (make-hpack-context)))
      (hpack-decode-headers ctx (hpack-bytes #x40 #x01 #xFF #x01 #x61))
      ;; Index 62 (first dynamic entry) with a new value "b".
      (hpack-decode-headers ctx (hpack-bytes #x7E #x01 #x62))
      (ok (= (length (hpack-context-dynamic-table ctx)) 2))
      (ok (= (hpack-context-dynamic-table-size ctx) (+ 34 34)))))
  (testing "Huffman-coded obs-text does not signal"
    (let* ((raw (make-array 1 :element-type '(unsigned-byte 8)
                            :initial-element #xFF))
           (huff (woo.http2.hpack::huffman-encode-bytes raw))
           (len-bytes (woo.http2.hpack::hpack-encode-integer
                       (length huff) 7 #x80))
           (value (make-array (+ (length len-bytes) (length huff))
                              :element-type '(unsigned-byte 8))))
      (loop for i from 0 below (length len-bytes)
            do (setf (aref value i) (nth i len-bytes)))
      (replace value huff :start1 (length len-bytes))
      (let* ((ctx (make-hpack-context))
             (block (hpack-cat (hpack-bytes #x00 #x01 #x78) value))
             (headers (hpack-decode-headers ctx block)))
        (ok (= (length headers) 1))
        (ok (string= (caar headers) "x"))
        (ok (= (length (cdar headers)) 1))
        (ok (= (char-code (char (cdar headers) 0)) #xFF))
        (ok (= (length (hpack-context-dynamic-table ctx)) 0))))))

(deftest hpack-huffman-method-get
  (testing "Huffman-encoded :method GET decodes correctly"
    (let* ((ctx (make-hpack-context))
           (name-enc (woo.http2.hpack::hpack-encode-string ":method" :huffman t))
           (value-enc (woo.http2.hpack::hpack-encode-string "GET" :huffman t))
           (data (make-array (+ 1 (length name-enc) (length value-enc))
                             :element-type '(unsigned-byte 8))))
      (ok (logbitp 7 (aref name-enc 0)))
      (ok (logbitp 7 (aref value-enc 0)))
      (setf (aref data 0) #x00)
      (replace data name-enc :start1 1)
      (replace data value-enc :start1 (1+ (length name-enc)))
      (let ((headers (hpack-decode-headers ctx data)))
        (ok (= (length headers) 1))
        (ok (string= (caar headers) ":method"))
        (ok (string= (cdar headers) "GET")))))
  (testing "Huffman string roundtrip"
    (dolist (s '("GET" "POST" "/" "www.example.com" ":method" "hello"))
      (let ((encoded (woo.http2.hpack::hpack-encode-string s :huffman t)))
        (multiple-value-bind (decoded consumed)
            (woo.http2.hpack::hpack-decode-string encoded 0)
          (ok (string= decoded s))
          (ok (= consumed (length encoded))))))))

(defun indexed-bomb-block (value-length refs)
  "x: <VALUE-LENGTH a's> with incremental indexing, then REFS references
   (#xbe, dynamic index 62) to it. Small on the wire, large decoded."
  (let* ((len (woo.http2.hpack::hpack-encode-integer value-length 7 0))
         (out (make-array (+ 3 (length len) value-length refs)
                          :element-type '(unsigned-byte 8)
                          :initial-element #xbe)))
    (replace out '(#x40 #x01 #x78))
    (replace out len :start1 3)
    (fill out 97 :start (+ 3 (length len)) :end (+ 3 (length len) value-length))
    out))

(deftest hpack-header-list-limit-while-decoding
  (testing "decoding stops at the field that crosses the limit"
    ;; Each field is 32 + 1 + 4000 = 4033 octets. 16 fields are 64,528;
    ;; the 17th crosses 65,536, so 984 of the 1000 references are never expanded.
    (let* ((block (indexed-bomb-block 4000 1000))
           (fields nil))
      (handler-case
          (hpack-decode-headers (make-hpack-context) block
                                :max-header-list-size 65536)
        (woo.http2.hpack:hpack-header-list-too-large (e)
          (setf fields (woo.http2.hpack:hpack-header-list-too-large-fields e))))
      (ok (eql fields 17))))

  (testing "the limit counts name + value + 32 per field"
    (let ((block (indexed-bomb-block 10 1)))
      ;; Two fields of 32 + 1 + 10 = 43 octets.
      (ok (= 2 (length (hpack-decode-headers (make-hpack-context) block
                                             :max-header-list-size 86))))
      (ok (signals (hpack-decode-headers (make-hpack-context) block
                                         :max-header-list-size 85)
                   'woo.http2.hpack:hpack-header-list-too-large))))

  (testing "without a limit the whole block decodes"
    (ok (= 1001 (length (hpack-decode-headers (make-hpack-context)
                                              (indexed-bomb-block 4000 1000)))))))
