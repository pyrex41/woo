(in-package :cl-user)
(defpackage woo-test.mutate
  (:use :cl :rove)
  (:import-from :woo.http2.hpack
                :make-hpack-context
                :hpack-decode-headers
                :hpack-compression-error))
(in-package :woo-test.mutate)

;; Mutation testing. Each mutant is one source edit that should make the
;; oracle fail. A mutant that still passes means the oracle does not lock
;; that behavior. The original file is compiled back in before returning.

(defvar *woo-root*
  (let ((here (or *load-truename* *compile-file-truename*)))
    (dotimes (i 2)
      (setf here (uiop:pathname-parent-directory-pathname here)))
    here))

(defun hpack-source ()
  (merge-pathnames "src/http2/hpack.lisp"
                   (or (ignore-errors (asdf:system-source-directory :woo))
                       *woo-root*)))

(defun octets (&rest bytes)
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun oracle ()
  "T when the loaded HPACK implementation still matches the spec points
   these mutants are aimed at."
  (handler-case
      (let ((ctx (make-hpack-context)))
        (multiple-value-bind (n c)
            (woo.http2.hpack::hpack-decode-integer (octets #x1f #x9a #x0a) 0 5)
          (unless (and (= n 1337) (= c 3))
            (return-from oracle nil)))
        (let ((dec (woo.http2.hpack::huffman-decode-bytes
                    (octets #xf1 #xe3 #xc2 #xe5 #xf2 #x3a #x6b #xa0 #xab #x90 #xf4 #xff)
                    0 12)))
          (unless (equalp dec (map 'vector #'char-code "www.example.com"))
            (return-from oracle nil)))
        ;; Index 2 is :method GET. Index 0 and a missing dynamic index error.
        (unless (equal (hpack-decode-headers ctx (octets #x82))
                       '((":method" . "GET")))
          (return-from oracle nil))
        ;; Index 61 is the last static entry. A <= / < mixup drops it.
        (unless (equal (hpack-decode-headers ctx (octets #xbd))
                       '(("www-authenticate" . "")))
          (return-from oracle nil))
        (handler-case
            (progn (hpack-decode-headers (make-hpack-context) (octets #xbe))
                   (return-from oracle nil))
          (hpack-compression-error ()))
        (handler-case
            (progn (hpack-decode-headers (make-hpack-context) (octets #x80))
                   (return-from oracle nil))
          (hpack-compression-error ()))
        (handler-case
            (progn (woo.http2.hpack::hpack-lookup-index (make-hpack-context) 0)
                   (return-from oracle nil))
          (hpack-compression-error ()))
        ;; An entry bigger than the table empties it.
        (let ((big (make-hpack-context :max-dynamic-table-size 40)))
          (woo.http2.hpack::hpack-context-add-entry big "a" "b")
          (woo.http2.hpack::hpack-context-add-entry big "this-name-is-far-too-long" "v")
          (unless (zerop (length (woo.http2.hpack:hpack-context-dynamic-table big)))
            (return-from oracle nil)))
        t)
    (error () nil)))

(defparameter *mutants*
  '(("multi-byte integer stops on the continuation bit"
     "(unless (logbitp 7 byte)
                   (return))"
     "(when (logbitp 7 byte)
                   (return))")
    ("valid huffman padding rejected"
     "(unless (= (logand last mask) mask)"
     "(unless (/= (logand last mask) mask)")
    ("static index 61 treated as dynamic"
     "((<= index static-len)
       (aref *static-table* index))"
     "((< index static-len)
       (aref *static-table* index))")
    ("unknown index accepted"
     "(unless (and (>= dyn-idx 0) (< dyn-idx (length dyn-table)))
           (hpack-error (format nil \"unknown index ~A\" index)))"
     "(when nil
           (hpack-error (format nil \"unknown index ~A\" index)))")
    ("oversized insert does not clear the table"
     "((> size (hpack-context-max-dynamic-table-size ctx))
       (hpack-context-clear ctx))"
     "((> size (hpack-context-max-dynamic-table-size ctx))
       nil)")))

(defun file-string (path)
  (uiop:read-file-string path))

(defun replace-first (text old new)
  (let ((pos (search old text)))
    (unless pos
      (error "mutant pattern missing"))
    (concatenate 'string
                 (subseq text 0 pos)
                 new
                 (subseq text (+ pos (length old))))))

(define-condition invalid-mutant (error)
  ((label :initarg :label :reader invalid-mutant-label)
   (reason :initarg :reason :reader invalid-mutant-reason))
  (:report (lambda (c s)
             (format s "mutant ~S did not compile/load: ~A"
                     (invalid-mutant-label c) (invalid-mutant-reason c)))))

(defun compile-and-load (src fasl label)
  "Compile SRC to FASL and load it. Any compile error, failure-p result, or
   load error signals INVALID-MUTANT: a mutant that does not build is a bug in
   the mutant string, not a kill."
  (handler-case
      (multiple-value-bind (out warnings-p failure-p)
          (compile-file src :output-file fasl :verbose nil :print nil)
        (declare (ignore warnings-p))
        (when (or (null out) failure-p)
          (error 'invalid-mutant :label label :reason "compile-file failed"))
        ;; Reloading redefines every HPACK function; that is the point.
        (handler-bind (#+sbcl (sb-kernel:redefinition-warning #'muffle-warning))
          (load out)))
    (invalid-mutant (c) (error c))
    (error (e)
      (error 'invalid-mutant :label label :reason (princ-to-string e)))))

(defun compile-string (text dir name label)
  (let* ((src (merge-pathnames (make-pathname :name name :type "lisp") dir))
         (fasl (make-pathname :type "fasl" :defaults src)))
    (with-open-file (out src :direction :output :if-exists :supersede)
      (write-string text out))
    (compile-and-load src fasl label)))

(defun restore-original (dir)
  "Compile the untouched on-disk source (fasl into DIR) and load it."
  (compile-and-load (hpack-source)
                    (merge-pathnames "hpack-restore.fasl" dir)
                    "original"))

(defun run-mutant (text old new label dir index)
  "Load one mutant, run the oracle, and always restore the original code.
   Returns :killed, :survived, :pattern-missing, or :invalid (with the
   condition as the second value)."
  (unless (search old text)
    (return-from run-mutant :pattern-missing))
  (let ((result nil)
        (invalid nil))
    (unwind-protect
         (handler-case
             (progn
               (compile-string (replace-first text old new) dir
                               (format nil "hpack-mutant-~D" index) label)
               (setf result (if (oracle) :survived :killed)))
           (invalid-mutant (c)
             (setf result :invalid invalid c)))
      (restore-original dir))
    (values result invalid)))

(deftest mutation-kills-hpack-mutants
  (ok (oracle) "unmutated HPACK passes the oracle")
  (let* ((source (hpack-source))
         (original (file-string source))
         (survived nil)
         (invalid nil)
         (killed 0))
    (woo-test.prop:with-unique-temp-directory (dir "mutate")
      (loop for (label old new) in *mutants*
            for index from 0
            do (multiple-value-bind (result condition)
                   (run-mutant original old new label dir index)
                 (ecase result
                   (:killed (incf killed))
                   (:survived (push label survived))
                   (:pattern-missing (push (list label :pattern-missing) survived))
                   (:invalid (push (princ-to-string condition) invalid)))
                 (ok (oracle)
                     (format nil "original HPACK restored after mutant ~S" label)))))
    (ok (string= original (file-string source))
        "on-disk HPACK source unchanged by mutation run")
    (ok (null invalid)
        (format nil "invalid (non-compiling) mutants: ~S" invalid))
    (ok (null survived)
        (format nil "surviving mutants: ~S" survived))
    (ok (= killed (length *mutants*))
        (format nil "~A/~A mutants killed by the oracle" killed (length *mutants*)))))
