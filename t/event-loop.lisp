(defpackage woo-test.event-loop
  (:use :cl :rove))
(in-package :woo-test.event-loop)

#+sbcl
(deftest event-loop-backend-descriptors-close
  "libev's loop destructor releases its kernel backend descriptors."
  (let ((before (length (directory #P"/dev/fd/*"))))
    (dotimes (i 12)
      (declare (ignore i))
      (woo.ev:with-event-loop ()))
    (dotimes (i 12)
      (declare (ignore i))
      (handler-case
          (woo.ev:with-event-loop (:cleanup-fn (lambda () (error "cleanup failed"))))
        (error () nil)))
    (let ((after (length (directory #P"/dev/fd/*"))))
      (ok (<= after (1+ before))
          (format nil "descriptors before=~D after=~D" before after)))))
