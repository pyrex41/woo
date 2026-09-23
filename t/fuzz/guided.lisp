(in-package :cl-user)
(defpackage woo-test.fuzz
  (:use :cl :rove)
  (:import-from :woo.http2.hpack :hpack-compression-error)
  (:import-from :woo.http2.frames :parse-frame :make-data-frame :serialize-frame))
(in-package :woo-test.fuzz)

;; Coverage-guided fuzzing of the pure codecs. SBCL sb-cover records which
;; source branches ran. An input is kept when it hits a branch the corpus has
;; not hit. The campaign seed is fixed so the run is reproducible.
;; WOO_FUZZ_ITERS overrides the mutation budget (default 60). SBCL only: other
;; implementations report a rove skip. The instrumented build lives in a
;; unique temp directory and the plain build is always reloaded afterwards.

(defun fuzz-iters ()
  (let ((env (ignore-errors (uiop:getenv "WOO_FUZZ_ITERS"))))
    (if (and env (plusp (length env)))
        (max 1 (parse-integer env :junk-allowed t))
        60)))

(defvar *woo-root*
  (let ((here (or *load-truename* *compile-file-truename*)))
    (dotimes (i 2)
      (setf here (uiop:pathname-parent-directory-pathname here)))
    here))

(defun source-files ()
  (let ((root (or (ignore-errors (asdf:system-source-directory :woo))
                  *woo-root*)))
    (mapcar (lambda (name) (merge-pathnames name root))
            '("src/http2/constants.lisp"
              "src/http2/frames.lisp"
              "src/http2/hpack.lisp"))))

(defun sb-cover-symbol (name)
  (or (find-symbol name :sb-cover)
      (error "sb-cover has no ~A" name)))

#+sbcl
(defun compile-codecs (dir &key coverage)
  "Compile and load the codec sources into DIR. The coverage policy is bound
   with WITH-COMPILATION-UNIT :POLICY for this extent only, never proclaimed
   globally."
  (require :sb-cover)
  (let ((quality (sb-cover-symbol "STORE-COVERAGE-DATA")))
    (with-compilation-unit (:policy `(optimize (,quality ,(if coverage 3 0))))
      (dolist (src (source-files))
        (let ((fasl (merge-pathnames
                     (make-pathname :name (format nil "~A-~:[plain~;cover~]"
                                                  (pathname-name src) coverage)
                                    :type "fasl")
                     dir)))
          (compile-file src :output-file fasl :verbose nil :print nil)
          (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning)
                         (error
                          (lambda (c)
                            (when (and (search "constant" (princ-to-string c))
                                       (find-restart 'continue c))
                              (invoke-restart (find-restart 'continue c))))))
            (load fasl)))))))

#+sbcl
(defun current-policy ()
  ;; Proclaim replaces the policy object, so EQ detects any global change.
  sb-c::*policy*)

(defun coverage-key ()
  (let ((get-coverage (find-symbol "GET-COVERAGE" :sb-cover)))
    (list (funcall get-coverage #'woo.http2.hpack::hpack-decode-headers)
          (funcall get-coverage #'woo.http2.hpack::hpack-decode-integer)
          (funcall get-coverage #'woo.http2.hpack::huffman-decode-bytes)
          (funcall get-coverage #'woo.http2.frames::parse-frame))))

(defun ub8 (list)
  (coerce list '(simple-array (unsigned-byte 8) (*))))

(defun execute (bytes)
  "Run both codecs. :crash for an unexpected error, otherwise :ok."
  (handler-case
      (woo.http2.hpack:hpack-decode-headers
       (woo.http2.hpack:make-hpack-context) bytes)
    (hpack-compression-error () nil)
    (error () (return-from execute :crash)))
  (handler-case
      (parse-frame bytes)
    (error () (return-from execute :crash)))
  :ok)

(defun interesting-bytes ()
  (ub8 '(0 1 #x7f #x80 #xff 10 31 127)))

(defun mutate (rng bytes corpus)
  (let ((op (mod (woo-test.prop:rng-next rng) 5))
        (v (make-array (length bytes) :element-type '(unsigned-byte 8)
                       :initial-contents (coerce bytes 'list))))
    (case op
      (0 (when (plusp (length v))
           (let ((i (mod (woo-test.prop:rng-next rng) (length v))))
             (setf (aref v i)
                   (logxor (aref v i)
                           (ash 1 (mod (woo-test.prop:rng-next rng) 8)))))))
      (1 (let ((extra (woo-test.prop:rng-uint rng 256)))
           (setf v (concatenate '(simple-array (unsigned-byte 8) (*))
                                v (ub8 (list extra))))))
      (2 (when (plusp (length v))
           (let ((i (mod (woo-test.prop:rng-next rng) (length v))))
             (setf v (concatenate '(simple-array (unsigned-byte 8) (*))
                                  (subseq v 0 i) (subseq v (1+ i)))))))
      (3 (when (plusp (length v))
           (setf (aref v (mod (woo-test.prop:rng-next rng) (length v)))
                 (aref (interesting-bytes)
                       (mod (woo-test.prop:rng-next rng) (length (interesting-bytes)))))))
      (4 (let ((other (nth (mod (woo-test.prop:rng-next rng) (length corpus)) corpus)))
           (setf v (concatenate '(simple-array (unsigned-byte 8) (*))
                                (subseq v 0 (floor (length v) 2))
                                (subseq other 0 (floor (length other) 2)))))))
    v))

(defun seed-corpus ()
  (list (ub8 '())
        (ub8 '(#x0a))
        (ub8 '(#x1f #x9a #x0a))
        (ub8 '(#xf1 #xe3 #xc2 #xe5 #xf2 #x3a #x6b #xa0 #xab #x90 #xf4 #xff))
        (serialize-frame
         (make-data-frame 1 (ub8 '(1 2 3)) :end-stream t))))

#+sbcl
(defun run-campaign ()
  (funcall (sb-cover-symbol "RESET-COVERAGE"))
  (let ((seen (make-hash-table :test 'equal))
        (corpus nil)
        (from-mutation 0)
        (crashes nil)
        (rng (woo-test.prop:make-prop-rng 1)))
    (dolist (seed (seed-corpus))
      (funcall (sb-cover-symbol "RESET-COVERAGE"))
      (let ((status (execute seed)))
        (when (eq status :crash)
          (push seed crashes))
        (setf (gethash (coverage-key) seen) t)
        (push seed corpus)))
    (let ((seed-cover (hash-table-count seen)))
      (loop repeat (fuzz-iters)
            for parent = (nth (mod (woo-test.prop:rng-next rng) (length corpus)) corpus)
            for child = (mutate rng parent corpus)
            do (funcall (sb-cover-symbol "RESET-COVERAGE"))
               (let ((status (execute child))
                     (key (coverage-key)))
                 (when (eq status :crash)
                   (push child crashes))
                 (unless (gethash key seen)
                   (setf (gethash key seen) t)
                   (push child corpus)
                   (incf from-mutation))))
      (ok (null crashes) "fuzz inputs must not crash outside compression errors")
      (ok (> seed-cover 1) "seeds themselves cover more than one path")
      (ok (> from-mutation 0)
          (format nil "mutations discovered ~A new coverage paths" from-mutation))
      (ok (> (hash-table-count seen) seed-cover)))))

(deftest coverage-guided-fuzz
  #+sbcl
  (progn
    (require :sb-cover)
    (let ((policy-before (current-policy)))
      (woo-test.prop:with-unique-temp-directory (dir "fuzz")
        (unwind-protect
             (progn
               (compile-codecs dir :coverage t)
               (run-campaign))
          ;; Always put uninstrumented production code back.
          (compile-codecs dir :coverage nil)
          (let ((clear (find-symbol "CLEAR-COVERAGE" :sb-cover)))
            (when clear (funcall clear)))))
      (ok (eq policy-before (current-policy))
          "global compiler policy unchanged by the coverage campaign")
      (ok (equal (woo.http2.hpack:hpack-decode-headers
                  (woo.http2.hpack:make-hpack-context) (ub8 '(#x82)))
                 '((":method" . "GET")))
          "reloaded uninstrumented HPACK still decodes")))
  #-sbcl
  (skip "coverage-guided fuzz needs SBCL sb-cover; skipped on this implementation"))
