(in-package :cl-user)
(defpackage woo-test.alpn
  (:use :cl :rove)
  (:import-from :woo.ssl.alpn
                :+ssl-tlsext-err-ok+
                :+ssl-tlsext-err-alert-fatal+
                :+ssl-tlsext-err-noack+
                :make-alpn-selector)
  (:import-from :cffi
                :with-foreign-object
                :mem-aref
                :foreign-alloc
                :foreign-free))
(in-package :woo-test.alpn)

;;; Test ALPN Constants

(deftest test-alpn-constants
  (testing "ALPN error constants have correct values"
    (ok (= +ssl-tlsext-err-ok+ 0))
    (ok (= +ssl-tlsext-err-alert-fatal+ 2))
    (ok (= +ssl-tlsext-err-noack+ 3))))

;;; Test parse-alpn-protocols

(deftest test-parse-alpn-protocols-empty
  (testing "parse-alpn-protocols handles empty buffer"
    (cffi:with-foreign-object (data :unsigned-char 0)
      (let ((result (woo.ssl.alpn::parse-alpn-protocols data 0)))
        (ok (null result))))))

(deftest test-parse-alpn-protocols-single
  (testing "parse-alpn-protocols parses single protocol"
    ;; Wire format for "h2": [2, 'h', '2']
    (let ((buffer #(2 104 50)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (let ((result (woo.ssl.alpn::parse-alpn-protocols data (length buffer))))
          (ok (equal result '("h2"))))))))

(deftest test-parse-alpn-protocols-multiple
  (testing "parse-alpn-protocols parses multiple protocols"
    ;; Wire format for ["h2", "http/1.1"]:
    ;; [2, 'h', '2', 8, 'h', 't', 't', 'p', '/', '1', '.', '1']
    (let ((buffer #(2 104 50 8 104 116 116 112 47 49 46 49)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (let ((result (woo.ssl.alpn::parse-alpn-protocols data (length buffer))))
          (ok (equal result '("h2" "http/1.1"))))))))

(deftest test-parse-alpn-protocols-three-protocols
  (testing "parse-alpn-protocols parses three protocols"
    ;; Wire format for ["h2", "http/1.1", "h3"]:
    ;; [2, 'h', '2', 8, 'h', 't', 't', 'p', '/', '1', '.', '1', 2, 'h', '3']
    (let ((buffer #(2 104 50 8 104 116 116 112 47 49 46 49 2 104 51)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (let ((result (woo.ssl.alpn::parse-alpn-protocols data (length buffer))))
          (ok (equal result '("h2" "http/1.1" "h3"))))))))

(deftest test-parse-alpn-protocols-zero-length-protocol
  (testing "parse-alpn-protocols skips zero-length protocol"
    ;; Wire format with zero-length protocol: [0, 2, 'h', '2']
    (let ((buffer #(0 2 104 50)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (let ((result (woo.ssl.alpn::parse-alpn-protocols data (length buffer))))
          (ok (equal result '("h2"))))))))

(deftest test-parse-alpn-protocols-truncated
  (testing "parse-alpn-protocols handles truncated buffer gracefully"
    ;; Wire format claims length 10 but only 5 bytes follow: [10, 'h', '2', ...]
    (let ((buffer #(10 104 50)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (let ((result (woo.ssl.alpn::parse-alpn-protocols data (length buffer))))
          ;; Should skip the invalid protocol
          (ok (null result)))))))

(deftest test-parse-alpn-protocols-truncated-after-valid
  (testing "a truncated entry does not discard an earlier protocol or read past inlen"
    ;; ["h2", truncated "http/1.1"]: length byte 8 but only two payload bytes.
    (let ((buffer #(2 104 50 8 104 116 116 112 47 49 46 49)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (let ((result (woo.ssl.alpn::parse-alpn-protocols data 6)))
          (ok (equal result '("h2"))))))))

;;; Test find-protocol-in-buffer

(deftest test-find-protocol-in-buffer-found
  (testing "find-protocol-in-buffer finds existing protocol"
    ;; Wire format for ["h2", "http/1.1"]
    (let ((buffer #(2 104 50 8 104 116 116 112 47 49 46 49)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (multiple-value-bind (ptr len)
            (woo.ssl.alpn::find-protocol-in-buffer data (length buffer) "h2")
          (ok (not (null ptr)))
          (ok (= len 2))
          ;; Verify the pointer points to the correct location
          (ok (= (cffi:mem-aref ptr :unsigned-char 0) 104)) ; 'h'
          (ok (= (cffi:mem-aref ptr :unsigned-char 1) 50))))))) ; '2'

(deftest test-find-protocol-in-buffer-second-protocol
  (testing "find-protocol-in-buffer finds second protocol"
    ;; Wire format for ["h2", "http/1.1"]
    (let ((buffer #(2 104 50 8 104 116 116 112 47 49 46 49)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (multiple-value-bind (ptr len)
            (woo.ssl.alpn::find-protocol-in-buffer data (length buffer) "http/1.1")
          (ok (not (null ptr)))
          (ok (= len 8))
          ;; Verify first few characters
          (ok (= (cffi:mem-aref ptr :unsigned-char 0) 104)) ; 'h'
          (ok (= (cffi:mem-aref ptr :unsigned-char 1) 116)) ; 't'
          (ok (= (cffi:mem-aref ptr :unsigned-char 2) 116))))))) ; 't'

(deftest test-find-protocol-in-buffer-not-found
  (testing "find-protocol-in-buffer returns nil when protocol not found"
    ;; Wire format for ["h2", "http/1.1"]
    (let ((buffer #(2 104 50 8 104 116 116 112 47 49 46 49)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (multiple-value-bind (ptr len)
            (woo.ssl.alpn::find-protocol-in-buffer data (length buffer) "h3")
          (ok (null ptr))
          (ok (null len)))))))

(deftest test-find-protocol-in-buffer-empty
  (testing "find-protocol-in-buffer handles empty buffer"
    (cffi:with-foreign-object (data :unsigned-char 0)
      (multiple-value-bind (ptr len)
          (woo.ssl.alpn::find-protocol-in-buffer data 0 "h2")
        (ok (null ptr))
        (ok (null len))))))

(deftest test-find-protocol-in-buffer-partial-match
  (testing "find-protocol-in-buffer doesn't match partial strings"
    ;; Wire format for ["http/1.1"]
    (let ((buffer #(8 104 116 116 112 47 49 46 49)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (multiple-value-bind (ptr len)
            (woo.ssl.alpn::find-protocol-in-buffer data (length buffer) "http")
          ;; Should not match "http" when buffer contains "http/1.1"
          (ok (null ptr))
          (ok (null len)))))))

(deftest test-find-protocol-in-buffer-truncated-entry
  (testing "find-protocol-in-buffer does not read past inlen"
    ;; Full allocation is ["h3","h2"]. inlen 4 keeps "h3" and the next
    ;; length byte; the "h2" payload sits past inlen. Matching it would
    ;; mean the scanner read outside the buffer.
    (let ((buffer #(2 104 51 2 104 50)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (multiple-value-bind (ptr len)
            (woo.ssl.alpn::find-protocol-in-buffer data 4 "h2")
          (ok (null ptr) "truncated h2 is not a match")
          (ok (null len)))
        (multiple-value-bind (ptr len)
            (woo.ssl.alpn::find-protocol-in-buffer data 4 "h3")
          (ok (not (null ptr)) "in-bounds h3 is still found")
          (ok (= len 2)))))
    ;; Sole entry claims 2 payload bytes but inlen stops one short,
    ;; and the completing byte is allocated just past inlen.
    (let ((buffer #(2 104 50)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (multiple-value-bind (ptr len)
            (woo.ssl.alpn::find-protocol-in-buffer data 2 "h2")
          (ok (null ptr))
          (ok (null len)))))))

;;; Test make-alpn-selector

(deftest test-make-alpn-selector-basic
  (testing "make-alpn-selector creates working selector"
    (let ((selector (make-alpn-selector '("h2" "http/1.1"))))
      (ok (functionp selector))
      ;; Test selecting first preference
      (let ((result (funcall selector '("h2" "http/1.1"))))
        (ok (string= result "h2")))
      ;; Test selecting second preference when first not available
      (let ((result (funcall selector '("http/1.1" "h3"))))
        (ok (string= result "http/1.1"))))))

(deftest test-make-alpn-selector-order-preference
  (testing "make-alpn-selector respects server preference order"
    (let ((selector (make-alpn-selector '("h2" "http/1.1"))))
      ;; Even if client prefers http/1.1, server prefers h2
      (let ((result (funcall selector '("http/1.1" "h2"))))
        (ok (string= result "h2"))))))

(deftest test-make-alpn-selector-no-match
  (testing "make-alpn-selector returns nil when no match"
    (let ((selector (make-alpn-selector '("h2" "http/1.1"))))
      (let ((result (funcall selector '("h3" "spdy/3"))))
        (ok (null result))))))

(deftest test-make-alpn-selector-empty-client-list
  (testing "make-alpn-selector handles empty client list"
    (let ((selector (make-alpn-selector '("h2" "http/1.1"))))
      (let ((result (funcall selector '())))
        (ok (null result))))))

(deftest test-make-alpn-selector-empty-preferred-list
  (testing "make-alpn-selector with empty preference list returns nil"
    (let ((selector (make-alpn-selector '())))
      (let ((result (funcall selector '("h2" "http/1.1"))))
        (ok (null result))))))

(deftest test-make-alpn-selector-single-protocol
  (testing "make-alpn-selector works with single protocol"
    (let ((selector (make-alpn-selector '("http/1.1"))))
      ;; Match
      (let ((result (funcall selector '("http/1.1"))))
        (ok (string= result "http/1.1")))
      ;; No match
      (let ((result (funcall selector '("h2"))))
        (ok (null result))))))

(deftest test-make-alpn-selector-case-sensitive
  (testing "make-alpn-selector is case-sensitive"
    (let ((selector (make-alpn-selector '("h2"))))
      (let ((result (funcall selector '("H2"))))
        ;; Should not match due to case difference
        (ok (null result))))))

(deftest test-make-alpn-selector-multiple-matches
  (testing "make-alpn-selector returns first match in preference order"
    (let ((selector (make-alpn-selector '("h3" "h2" "http/1.1"))))
      ;; Client supports h2 and http/1.1, but server prefers h2
      (let ((result (funcall selector '("http/1.1" "h2"))))
        (ok (string= result "h2")))
      ;; Client supports all, server picks first preference
      (let ((result (funcall selector '("http/1.1" "h2" "h3"))))
        (ok (string= result "h3"))))))

;;; Test woo.ssl package

(deftest test-alpn-protocols-default
  (testing "*alpn-protocols* has correct default value"
    (when (find-package :woo.ssl)
      (ok (equal (symbol-value (find-symbol "*ALPN-PROTOCOLS*" :woo.ssl))
                 '("http/1.1"))))))

(deftest test-alpn-callback-arg-roundtrip
  (testing "preferred protocols encoded in a private callback arg"
    (let ((first (woo.ssl.alpn::encode-protocols-arg '("h2")))
          (second (woo.ssl.alpn::encode-protocols-arg '("http/1.1"))))
      (unwind-protect
           (progn
             (ok (not (cffi:pointer-eq first second))
                 "each encode allocates its own buffer")
             (ok (equal (woo.ssl.alpn::protocols-from-arg first) '("h2"))
                 "earlier buffer is not freed by a later encode")
             (ok (equal (woo.ssl.alpn::protocols-from-arg second) '("http/1.1"))))
        (cffi:foreign-free first)
        (cffi:foreign-free second)))))

(deftest test-alpn-arg-per-context
  (testing "each SSL_CTX has its own callback arg and reconfigure does not free it"
    (let ((ctx-a (cffi:make-pointer #xF0000000A1100001))
          (ctx-b (cffi:make-pointer #xF0000000A1100002)))
      (let* ((a1 (woo.ssl.alpn::ensure-ctx-alpn-arg ctx-a '("h2" "http/1.1")))
             (a2 (woo.ssl.alpn::ensure-ctx-alpn-arg ctx-a '("h2" "http/1.1")))
             (b1 (woo.ssl.alpn::ensure-ctx-alpn-arg ctx-b '("http/1.1"))))
        (ok (cffi:pointer-eq a1 a2)
            "unchanged protocols keep the same pointer")
        (ok (not (cffi:pointer-eq a1 b1))
            "contexts do not share one C buffer")
        (ok (equal (woo.ssl.alpn::protocols-from-arg a1) '("h2" "http/1.1")))
        (ok (equal (woo.ssl.alpn::protocols-from-arg b1) '("http/1.1")))
        (let ((a3 (woo.ssl.alpn::ensure-ctx-alpn-arg ctx-a '("h2"))))
          (ok (not (cffi:pointer-eq a1 a3))
              "a new list gets a new buffer")
          (ok (equal (woo.ssl.alpn::protocols-from-arg a1) '("h2" "http/1.1"))
              "previous arg is still readable")
          (ok (equal (woo.ssl.alpn::protocols-from-arg a3) '("h2")))
          (ok (equal (woo.ssl.alpn::protocols-from-arg b1) '("http/1.1"))
              "the other context is untouched"))))))

(defun call-alpn-select (data inlen arg)
  (cffi:with-foreign-objects ((out :pointer)
                              (outlen :unsigned-char))
    (setf (cffi:mem-ref out :pointer) (cffi:null-pointer))
    (setf (cffi:mem-ref outlen :unsigned-char) 0)
    (let ((rc (cffi:foreign-funcall-pointer
               (cffi:callback woo.ssl.alpn::alpn-select-cb)
               ()
               :pointer (cffi:null-pointer)
               :pointer out
               :pointer outlen
               :pointer data
               :unsigned-int inlen
               :pointer arg
               :int)))
      (values rc
              (cffi:mem-ref outlen :unsigned-char)
              (cffi:mem-ref out :pointer)))))

(deftest test-alpn-callback-inbounds-and-truncated
  (testing "ALPN callback selects an in-bounds protocol and rejects a truncated list"
    (let ((client #(2 104 50 8 104 116 116 112 47 49 46 49))
          (arg (woo.ssl.alpn::encode-protocols-arg '("h2" "http/1.1"))))
      (unwind-protect
           (cffi:with-foreign-object (data :unsigned-char (length client))
             (loop for i from 0 below (length client)
                   do (setf (cffi:mem-aref data :unsigned-char i) (aref client i)))
             (multiple-value-bind (rc outlen out)
                 (call-alpn-select data (length client) arg)
               (ok (= rc +ssl-tlsext-err-ok+))
               (ok (= outlen 2))
               (ok (not (cffi:null-pointer-p out)))
               (when (and out (not (cffi:null-pointer-p out)))
                 (ok (= (cffi:mem-aref out :unsigned-char 0) 104))
                 (ok (= (cffi:mem-aref out :unsigned-char 1) 50)))))
        (cffi:foreign-free arg)))
    ;; inlen covers "h2" plus a length byte claiming 8 more bytes that are
    ;; not in range. Preferring only http/1.1 must not succeed by reading them.
    (let ((client #(2 104 50 8 104 116 116 112 47 49 46 49))
          (arg (woo.ssl.alpn::encode-protocols-arg '("http/1.1"))))
      (unwind-protect
           (cffi:with-foreign-object (data :unsigned-char (length client))
             (loop for i from 0 below (length client)
                   do (setf (cffi:mem-aref data :unsigned-char i) (aref client i)))
             (multiple-value-bind (rc outlen out)
                 (call-alpn-select data 6 arg)
               (ok (= rc +ssl-tlsext-err-noack+))
               (ok (= outlen 0) "truncated match must not publish a length")
               (ok (cffi:null-pointer-p out)
                   "truncated match must not publish a pointer past inlen")))
        (cffi:foreign-free arg)))))

(deftest test-alpn-truncated-allocation
  (testing "scanner and callback do not read past a truncated ALPN buffer"
    ;; Exactly four octets are allocated: "h2" and a length byte of 8.
    ;; Those eight payload bytes are not in the buffer. Selecting
    ;; http/1.1, or walking the claimed length, would read past it.
    (let ((buffer #(2 104 50 8)))
      (cffi:with-foreign-object (data :unsigned-char (length buffer))
        (loop for i from 0 below (length buffer)
              do (setf (cffi:mem-aref data :unsigned-char i) (aref buffer i)))
        (multiple-value-bind (ptr len)
            (woo.ssl.alpn::find-protocol-in-buffer data (length buffer) "http/1.1")
          (ok (null ptr))
          (ok (null len)))
        (multiple-value-bind (ptr len)
            (woo.ssl.alpn::find-protocol-in-buffer data (length buffer) "h2")
          (ok (not (null ptr)))
          (ok (= len 2))
          (ok (= (cffi:mem-aref ptr :unsigned-char 0) 104))
          (ok (= (cffi:mem-aref ptr :unsigned-char 1) 50)))
        (let ((prefer-h2 (woo.ssl.alpn::encode-protocols-arg '("h2" "http/1.1")))
              (prefer-11 (woo.ssl.alpn::encode-protocols-arg '("http/1.1"))))
          (unwind-protect
               (progn
                 (multiple-value-bind (rc outlen out)
                     (call-alpn-select data (length buffer) prefer-h2)
                   (ok (= rc +ssl-tlsext-err-ok+))
                   (ok (= outlen 2))
                   (ok (not (cffi:null-pointer-p out)))
                   (when (and out (not (cffi:null-pointer-p out)))
                     (ok (= (cffi:mem-aref out :unsigned-char 0) 104))
                     (ok (= (cffi:mem-aref out :unsigned-char 1) 50))))
                 (multiple-value-bind (rc outlen out)
                     (call-alpn-select data (length buffer) prefer-11)
                   (ok (= rc +ssl-tlsext-err-noack+))
                   (ok (= outlen 0))
                   (ok (cffi:null-pointer-p out))))
            (cffi:foreign-free prefer-h2)
            (cffi:foreign-free prefer-11)))))))

(deftest test-alpn-not-queried-before-handshake
  (testing "start-socket defers ALPN until after pending buffer / ssl-read path"
    (ok (fboundp 'woo:looks-like-http2-preface))
    (ok (fboundp 'woo:http2-connection-preface-match))
    (let* ((path (asdf:system-relative-pathname :woo "src/woo.lisp"))
           (src (uiop:read-file-string path))
           (start (search "(start-socket (socket)" src))
           (next (and start (search "(start-multithread-server" src :start2 start)))
           (body (and start next (subseq src start next)))
           (init-pos (and body (search "init-ssl-handle" body)))
           (pending-pos (and body (search "pending" body)))
           (alpn-pos (and body (search "get-negotiated-protocol" body))))
      (ok start "start-socket is defined")
      (ok (and init-pos pending-pos alpn-pos)
          "init-ssl-handle, pending buffer, and ALPN all present")
      (ok (< init-pos pending-pos)
          "pending buffer is created after init-ssl-handle")
      (ok (< pending-pos alpn-pos)
          "get-negotiated-protocol is after the pending buffer, not at handshake init"))))
