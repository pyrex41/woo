(in-package :cl-user)
(defpackage woo-test.showcase
  (:use :cl :rove)
  (:import-from :woo.websocket :websocket-p)
  (:import-from :woo-showcase.limits
                :*max-resource-delay-ms*
                :clamp-resource-delay
                :request-clack-env))
(in-package :woo-test.showcase)

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
  (ok (= (clamp-resource-delay "99999") *max-resource-delay-ms*)
      "huge delay clamped to max")
  (ok (= (clamp-resource-delay "not-a-number") 100) "junk falls back to default")
  (ok (= (clamp-resource-delay 1500) *max-resource-delay-ms*)
      "integer over max clamped"))

(deftest request-clack-env-plist
  (let ((env (make-ws-env)))
    (ok (eq (request-clack-env env) env) "plist env is returned as-is")
    (ok (websocket-p (request-clack-env env)) "websocket-p sees Clack plist")))

(defclass stand-in-request ()
  ((env :initarg :env :reader stand-in-request-env)))

(defun call-with-lack-request-maker (fn)
  "Call FN with a function that builds a Lack request from an env plist.
   Uses the real lack-request when it loads; otherwise (woo-test does not
   depend on it) a temporary LACK.REQUEST package whose REQUEST-ENV reads
   a stand-in object, removed afterwards."
  (if (or (find-package :lack.request)
          (ignore-errors (asdf:load-system "lack-request") t))
      (funcall fn (symbol-function (find-symbol "MAKE-REQUEST" :lack.request)))
      (let ((pkg (make-package :lack.request :use '())))
        (unwind-protect
             (progn
               (setf (symbol-function (intern "REQUEST-ENV" pkg))
                     #'stand-in-request-env)
               (funcall fn (lambda (env)
                             (make-instance 'stand-in-request :env env))))
          (delete-package pkg)))))

(deftest request-clack-env-from-lack-request
  ;; The showcase bug: the /ws/echo route passed ningle:*request* (a Lack
  ;; request object) straight to websocket-p and getf. request-clack-env
  ;; must recover the Clack env plist from it.
  (testing "websocket-p needs a plist; a request object is a type error"
    (ok (signals (websocket-p (make-instance 'standard-object)) 'type-error)))
  (testing "a Lack request yields its env, which websocket-p accepts"
    (call-with-lack-request-maker
     (lambda (make-request)
       (let* ((env (make-ws-env))
              (req (funcall make-request env)))
         (ok (not (consp req)) "the request is an object, not a plist")
         (ok (eq (request-clack-env req) env)
             "request-clack-env returns the request's env")
         (ok (websocket-p (request-clack-env req))
             "the upgrade is detected through a Lack request"))))))

(deftest default-backlog-is-128
  (ok (= woo::*default-backlog-size* 128)))
