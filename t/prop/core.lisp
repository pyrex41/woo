(in-package :cl-user)
(defpackage woo-test.prop
  (:use :cl :rove)
  (:export :*prop-iters*
           :*prop-seed*
           :make-prop-rng
           :rng-next
           :rng-uint
           :rng-int
           :rng-bool
           :rng-choose
           :check-property
           :shrink-int
           :shrink-list
           :shrink-vector
           :try-shrink
           :bytes-equal-p
           :prop-iters))
(in-package :woo-test.prop)

(defun prop-iters ()
  "Bounded iteration count. WOO_PROP_ITERS env, default 100."
  (let ((env (ignore-errors (uiop:getenv "WOO_PROP_ITERS"))))
    (if (and env (plusp (length env)))
        (max 1 (parse-integer env :junk-allowed t))
        100)))

(defvar *prop-iters* 100)
(defvar *prop-seed* 0)

(defstruct rng
  (state 0 :type (unsigned-byte 64)))

(defun make-prop-rng (seed)
  (make-rng :state (logand seed #xFFFFFFFFFFFFFFFF)))

(defun rng-next (rng)
  "xorshift64."
  (let ((x (rng-state rng)))
    (when (zerop x) (setf x 1))
    (setf x (logxor x (ash x 13)))
    (setf x (logand x #xFFFFFFFFFFFFFFFF))
    (setf x (logxor x (ash x -7)))
    (setf x (logand x #xFFFFFFFFFFFFFFFF))
    (setf x (logxor x (ash x 17)))
    (setf x (logand x #xFFFFFFFFFFFFFFFF))
    (setf (rng-state rng) x)
    x))

(defun rng-uint (rng n)
  "Uniform integer in [0, n) for n > 0."
  (if (<= n 1)
      0
      (mod (rng-next rng) n)))

(defun rng-int (rng lo hi)
  "Inclusive [lo, hi]."
  (+ lo (rng-uint rng (1+ (- hi lo)))))

(defun rng-bool (rng)
  (zerop (logand (rng-next rng) 1)))

(defun rng-choose (rng sequence)
  (elt sequence (rng-uint rng (length sequence))))

(defun shrink-int (n)
  (cond
    ((zerop n) nil)
    ((plusp n)
     (remove-duplicates
      (remove-if-not (lambda (x) (and (>= x 0) (< x n)))
                     (list 0 (floor n 2) (1- n) (min 1 (1- n))))))
    (t
     (list 0 (ceiling n 2) (1+ n)))))

(defun shrink-list (list shrink-elem)
  (let ((out nil)
        (len (length list)))
    (when (> len 0)
      (push nil out)
      (push (subseq list 0 (floor len 2)) out)
      (push (rest list) out)
      (push (butlast list) out))
    (loop for i from 0 below len
          for elem = (nth i list)
          do (dolist (s (funcall shrink-elem elem))
               (push (append (subseq list 0 i) (list s) (nthcdr (1+ i) list))
                     out)))
    (remove-duplicates out :test #'equal)))

(defun shrink-vector (vec)
  (let ((len (length vec)))
    (cond
      ((zerop len) nil)
      (t
       (list (subseq vec 0 0)
             (subseq vec 0 (floor len 2))
             (subseq vec 0 (max 0 (1- len))))))))

(defun bytes-equal-p (a b)
  (and (= (length a) (length b))
       (loop for i from 0 below (length a)
             always (= (aref a i) (aref b i)))))

(defun try-shrink (value shrinker property)
  (loop with current = value
        for depth from 0 below 64
        for candidates = (and shrinker (funcall shrinker current))
        do (let ((better nil))
             (dolist (c candidates)
               (handler-case
                   (unless (funcall property c)
                     (setf better c)
                     (return))
                 (error ()
                   (setf better c)
                   (return))))
             (if better
                 (setf current better)
                 (return current)))
        finally (return current)))

(defun check-property (name property generator &key (shrinker nil) (iters nil))
  "Run PROPERTY on GENERATOR samples. On failure print seed and shrunk value.
   PROPERTY is (lambda (value) ...) returning T on success.
   GENERATOR is (lambda (rng size) value)."
  (let* ((n (or iters (prop-iters)))
         (base-seed (logand (get-universal-time) #xFFFFFFFF)))
    (loop for i from 0 below n
          for seed = (logand (+ base-seed (* i 1103515245) 12345) #xFFFFFFFF)
          for rng = (make-prop-rng seed)
          for size = (1+ (mod i 16))
          for value = (funcall generator rng size)
          do (let ((ok t)
                   (err nil))
               (handler-case
                   (unless (funcall property value)
                     (setf ok nil))
                 (error (e)
                   (setf ok nil err e)))
               (unless ok
                 (let ((shrunk (try-shrink value shrinker property)))
                   (format t "~&Property ~A failed seed=~A iter=~A/~A~%  value=~S~%  shrunk=~S~%~@[  error=~A~%~]"
                           name seed i n value shrunk err)
                   (return-from check-property
                     (values nil seed shrunk err))))))
    t))
