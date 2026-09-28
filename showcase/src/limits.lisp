(in-package :cl-user)
(defpackage :woo-showcase.limits
  (:use :cl)
  (:export :*max-resource-delay-ms*
           :clamp-resource-delay
           :request-clack-env))
(in-package :woo-showcase.limits)

(defparameter *max-resource-delay-ms* 1000)

(defun clamp-resource-delay (query-delay)
  "Parse delay query (ms) and clamp to [0, *max-resource-delay-ms*]. Default 100."
  (let ((n (cond
             ((null query-delay) 100)
             ((integerp query-delay) query-delay)
             (t (or (ignore-errors
                      (parse-integer (princ-to-string query-delay) :junk-allowed t))
                    100)))))
    (max 0 (min *max-resource-delay-ms* n))))

(defun request-clack-env (request)
  "Ningle binds *request* to a Lack request object, not a Clack env plist."
  (cond
    ((consp request) request)
    ((and (find-package :lack.request)
          (fboundp (find-symbol "REQUEST-ENV" :lack.request)))
     (funcall (find-symbol "REQUEST-ENV" :lack.request) request))
    (t (error "Cannot extract Clack env from ~S" request))))
