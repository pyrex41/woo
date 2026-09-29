(in-package :cl-user)
(defpackage woo-test.qc.props
  (:use :cl :rove :woo-test.qc)
  (:import-from :woo.http2.hpack
                :make-hpack-context
                :hpack-encode-headers
                :hpack-decode-headers
                :hpack-compression-error))
(in-package :woo-test.qc.props)

(defun integer-roundtrip (value)
  (let* ((prefix 5)
         (bytes (woo.http2.hpack::hpack-encode-integer value prefix 0))
         (vec (coerce bytes '(simple-array (unsigned-byte 8) (*)))))
    (multiple-value-bind (decoded consumed)
        (woo.http2.hpack::hpack-decode-integer vec 0 prefix)
      (and (= decoded value) (= consumed (length vec))))))

(defun literal-incremental (name value)
  "One RFC 7541 §6.2.1 block: new name, no Huffman, incremental indexing."
  (let ((head (woo.http2.hpack::hpack-encode-integer 0 6 #x40))
        (name-bytes (woo.http2.hpack::hpack-encode-string name))
        (value-bytes (woo.http2.hpack::hpack-encode-string value)))
    (concatenate '(simple-array (unsigned-byte 8) (*))
                 (coerce head '(simple-array (unsigned-byte 8) (*)))
                 name-bytes
                 value-bytes)))

(deftest qc-integer-roundtrip
  (ok (qc-run "hpack-integer"
              (tuple (integer-between 0 100000))
              (lambda (vals)
                (let ((n (car vals)))
                  (classify (if (< n 31) "small" "large"))
                  (note "n" n)
                  (integer-roundtrip n)))
              :examples '((10) (1337) (42)))
      "Quickcheck integer encode/decode, with the RFC examples always replayed"))

(deftest hypothesis-string-roundtrip
  (ok (given ((s (ascii-string :max 20)))
        (assume (plusp (length s)))
        (note "s" s)
        (let* ((enc (woo.http2.hpack::hpack-encode-string s :huffman t))
               (vec (coerce enc '(simple-array (unsigned-byte 8) (*)))))
          (multiple-value-bind (decoded consumed)
              (woo.http2.hpack::hpack-decode-string vec 0)
            (and (string= decoded s) (= consumed (length vec))))))
      "Hypothesis-style assume/note over Huffman strings"))

(deftest qc-stateful-dynamic-table
  (ok (qc-run "dynamic-table-model"
              (tuple (ascii-string :max 12) (ascii-string :max 12)
                     (ascii-string :max 12) (ascii-string :max 12))
              (lambda (vals)
                (destructuring-bind (n1 v1 n2 v2) vals
                  (assume (and (plusp (length n1)) (plusp (length n2))))
                  (let ((ctx (make-hpack-context)))
                    (flet ((add-field (name value)
                             (let ((got (hpack-decode-headers
                                         ctx (literal-incremental name value))))
                               (equal got (list (cons name value))))))
                      (and (add-field n1 v1)
                           (add-field n2 v2)
                           ;; Index 62 is the newest dynamic entry.
                           (equal (hpack-decode-headers
                                   ctx (coerce #(#xbe) '(simple-array (unsigned-byte 8) (*))))
                                  (list (cons n2 v2))))))))
              :examples '(("custom-key" "custom-header" "a" "b")))
      "Stateful Quickcheck: incremental indexing matches a two-step model"))

(deftest qc-header-list-roundtrip
  (ok (for-all ((name (ascii-string :max 16))
                (value (ascii-string :max 16)))
        (assume (plusp (length name)))
        (let* ((headers (list (cons name value)))
               (ctx-e (make-hpack-context))
               (ctx-d (make-hpack-context)))
          (equal (hpack-decode-headers ctx-d (hpack-encode-headers ctx-e headers))
                 headers)))
      "for-all header list round-trip"))
