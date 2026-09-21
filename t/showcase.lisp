(in-package :cl-user)
(defpackage woo-test.showcase
  (:use :cl :rove)
  (:import-from :woo.websocket :websocket-p))
(in-package :woo-test.showcase)

;;; Spec copies of showcase helpers (woo-test does not load ningle/showcase).
;;; Keep in sync with showcase/src/app.lisp.

(defparameter *max-resource-delay-ms* 1000)

(defun clamp-resource-delay (query-delay)
  (let ((n (cond
             ((null query-delay) 100)
             ((integerp query-delay) query-delay)
             (t (or (ignore-errors
                      (parse-integer (princ-to-string query-delay) :junk-allowed t))
                    100)))))
    (max 0 (min *max-resource-delay-ms* n))))

(defun request-clack-env (request)
  (cond
    ((consp request) request)
    ((and (find-package :lack.request)
          (fboundp (find-symbol "REQUEST-ENV" :lack.request)))
     (funcall (find-symbol "REQUEST-ENV" :lack.request) request))
    (t (error "Cannot extract Clack env from ~S" request))))

(defun make-ws-env ()
  (let ((headers (make-hash-table :test 'equal)))
    (setf (gethash "upgrade" headers) "websocket"
          (gethash "connection" headers) "Upgrade"
          (gethash "sec-websocket-key" headers) "dGhlIHNhbXBsZSBub25jZQ=="
          (gethash "sec-websocket-version" headers) "13")
    (list :request-method :GET :headers headers :clack.io :socket)))

(deftest clamp-resource-delay-bounds
  (ok (= (clamp-resource-delay nil) 100) "default 100ms")
  (ok (= (clamp-resource-delay "250") 250) "numeric string")
  (ok (= (clamp-resource-delay "0") 0) "zero allowed")
  (ok (= (clamp-resource-delay "-5") 0) "negative clamped to 0")
  (ok (= (clamp-resource-delay "99999") 1000) "huge delay clamped to max")
  (ok (= (clamp-resource-delay "not-a-number") 100) "junk falls back to default")
  (ok (= (clamp-resource-delay 1500) 1000) "integer over max clamped"))

(deftest request-clack-env-plist
  (let ((env (make-ws-env)))
    (ok (eq (request-clack-env env) env) "plist env is returned as-is")
    (ok (websocket-p (request-clack-env env)) "websocket-p sees Clack plist")))

(deftest websocket-p-rejects-non-plist-request
  (testing "ningle:*request* is not a Clack env; getf on an object is invalid"
    (ok (not (ignore-errors (websocket-p (make-instance 'standard-object))))
        "websocket-p must not treat a request object as an env plist")))
