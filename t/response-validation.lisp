(in-package :cl-user)
(defpackage woo-test.response-validation (:use :cl :rove))
(in-package :woo-test.response-validation)

(deftest validates-response-shape-and-types
  (ok (not (handler-case (progn (woo::validate-response '(200 nil ("ok"))) nil)
             (error () t)))))

(deftest normalizes-without-interning
  (let* ((name (format nil "x-woo-validation-~A" (gensym)))
         (response (woo::validate-response (list 200 (list name "ok") "ok"))))
    (ok (stringp (first (second response))))
    (ok (null (find-symbol (string-upcase name) :keyword)))))

(deftest checks-content-length-and-generated-framing
  (ok (handler-case (progn (woo::validate-response '(200 (:content-length 2) ("bad"))) nil)
        (error () t)))
  (ok (null (getf (second (woo::validate-response '(200 (:transfer-encoding "chunked") ("ok"))))
               :transfer-encoding))
      "application transfer encoding is removed")
  (ok (woo::validate-response '(200 (:content-length 99) nil) t)
      "HEAD responses do not compare body length"))
