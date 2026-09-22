(in-package :cl-user)
(defpackage woo-test.qc
  (:use :cl :woo-test.prop)
  (:export :qc-gen
           :integer-between
           :ascii-string
           :byte-vector
           :elements
           :oneof
           :tuple
           :assume
           :note
           :classify
           :for-all
           :given
           :qc-run
           :*qc-seed*))
(in-package :woo-test.qc)

;; Quickcheck (for-all / generators / shrink) and Hypothesis (given / assume /
;; note / example replay) on the same runner. Seeds are deterministic unless
;; WOO_QC_SEED is set. Failures are appended to t/prop/.qc-failures and replayed
;; on the next run.

(defvar *qc-seed* nil)
(defvar *qc-notes* nil)
(defvar *qc-classes* nil)

(define-condition assumption-failed () ())

(defstruct qc-gen sample shrink)

(defun qc-seed ()
  (let ((env (ignore-errors (uiop:getenv "WOO_QC_SEED"))))
    (if (and env (plusp (length env)))
        (parse-integer env :junk-allowed t)
        (or *qc-seed* 20260922))))

(defun assume (ok)
  "Hypothesis assume / Quickcheck ==> . The trial is discarded, not failed."
  (unless ok
    (signal 'assumption-failed)))

(defun note (label value)
  "Recorded and printed with a failing trial."
  (push (cons label value) *qc-notes*))

(defun classify (label)
  (when *qc-classes*
    (incf (gethash label *qc-classes* 0))))

(defun integer-between (lo hi)
  (make-qc-gen
   :sample (lambda (rng size)
             (declare (ignore size))
             (rng-int rng lo hi))
   :shrink (lambda (n)
             (remove-if (lambda (x) (or (< x lo) (> x hi)))
                        (shrink-int n)))))

(defun ascii-string (&key (max 24))
  (make-qc-gen
   :sample (lambda (rng size)
             (let* ((n (rng-int rng 0 (min max (max 1 size))))
                    (s (make-string n)))
               (dotimes (i n)
                 (setf (char s i) (code-char (rng-int rng 97 122))))
               s))
   :shrink #'shrink-vector))

(defun byte-vector (&key (max 32))
  (make-qc-gen
   :sample (lambda (rng size)
             (let* ((n (rng-int rng 0 (min max (max 1 size))))
                    (v (make-array n :element-type '(unsigned-byte 8))))
               (dotimes (i n)
                 (setf (aref v i) (rng-uint rng 256)))
               v))
   :shrink #'shrink-vector))

(defun elements (items)
  (let ((vec (coerce items 'vector)))
    (make-qc-gen
     :sample (lambda (rng size)
               (declare (ignore size))
               (rng-choose rng vec))
     :shrink (constantly nil))))

(defun oneof (gens)
  (make-qc-gen
   :sample (lambda (rng size)
             (funcall (qc-gen-sample (rng-choose rng gens)) rng size))
   :shrink (lambda (value)
             (loop for g in gens
                   append (and (qc-gen-shrink g)
                               (funcall (qc-gen-shrink g) value))))))

(defun tuple (&rest gens)
  (make-qc-gen
   :sample (lambda (rng size)
             (mapcar (lambda (g) (funcall (qc-gen-sample g) rng size)) gens))
   :shrink (lambda (vals)
             (let ((out nil))
               (loop for v in vals
                     for g in gens
                     for i from 0
                     do (dolist (s (and (qc-gen-shrink g)
                                        (funcall (qc-gen-shrink g) v)))
                          (let ((copy (copy-list vals)))
                            (setf (nth i copy) s)
                            (push copy out))))
               out))))

(defvar *woo-root*
  (let ((here (or *load-truename* *compile-file-truename*)))
    (dotimes (i 2)
      (setf here (uiop:pathname-parent-directory-pathname here)))
    here))

(defun woo-root ()
  (or (ignore-errors (asdf:system-source-directory :woo))
      *woo-root*))

(defun failure-file ()
  (merge-pathnames "t/prop/.qc-failures" (woo-root)))

(defun read-failures (name)
  (let ((path (failure-file))
        (out nil))
    (when (probe-file path)
      (with-open-file (in path)
        (loop for form = (read in nil :eof)
              until (eq form :eof)
              do (when (and (consp form) (equal (car form) name))
                   (push (cdr form) out)))))
    out))

(defun record-failure (name value)
  (ignore-errors
    (with-open-file (out (failure-file)
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (prin1 (cons name value) out)
      (terpri out))))

(defun qc-run (name gens property &key examples (iters nil))
  "Run PROPERTY on samples of GENS. PROPERTY receives one list of values.
   Returns T, or NIL after printing the seed, notes, and shrunk arguments.
   Saved failures and EXAMPLES are always tried first (Hypothesis replay)."
  (let* ((n (or iters (prop-iters)))
         (base (qc-seed))
         (*qc-classes* (make-hash-table :test 'equal))
         (shrinker (qc-gen-shrink gens))
         (tried 0)
         (discards 0))
    (labels ((trial (value seed)
               (let ((*qc-notes* nil))
                 (handler-case
                     (funcall property value)
                   (assumption-failed ()
                     (incf discards)
                     :discard)
                   (error (e)
                     (fail value seed e))
                   (:no-error (ok)
                     (if ok
                         t
                         (fail value seed nil))))))
             (fail (value seed err)
               (let ((shrunk (try-shrink value shrinker
                                         (lambda (v)
                                           (handler-case
                                               (funcall property v)
                                             (assumption-failed () t)
                                             (error () nil))))))
                 (record-failure name shrunk)
                 (format t "~&~A failed seed=~A~%  value=~S~%  shrunk=~S~%  notes=~S~%~@[  error=~A~%~]"
                         name seed value shrunk (reverse *qc-notes*) err)
                 (return-from qc-run nil))))
      (dolist (saved (read-failures name))
        (trial saved :replay))
      (dolist (ex examples)
        (trial ex :example))
      (loop for i from 0 below (* n 20)
            while (< tried n)
            for seed = (logand (+ base (* i 1103515245) 12345) #xFFFFFFFF)
            for rng = (make-prop-rng seed)
            for size = (1+ (mod i 32))
            for value = (funcall (qc-gen-sample gens) rng size)
            for result = (trial value seed)
            do (when (eq result t)
                 (incf tried)))
      (when (zerop tried)
        (format t "~&~A discarded every trial (~A discards)~%" name discards)
        (return-from qc-run nil))
      t)))

(defmacro for-all (bindings &body body)
  "Quickcheck. BINDINGS are (var generator). BODY is the predicate."
  (let ((vals (gensym))
        (vars (mapcar #'car bindings))
        (gens (mapcar #'cadr bindings)))
    `(qc-run ,(format nil "~{~A~^,~}" vars)
             (tuple ,@gens)
             (lambda (,vals)
               (destructuring-bind ,vars ,vals
                 ,@body))
             :examples nil)))

(defmacro given (bindings &body body)
  "Hypothesis-shaped alias of for-all."
  `(for-all ,bindings ,@body))
