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

;;; The showcase itself (ningle, djula, jonathan) is not a woo-test
;;; dependency. Load it when it is available; otherwise skip visibly.

(defun showcase-loaded-p ()
  "Load woo-showcase from the repository's showcase/ directory. NIL if it
   (or one of its dependencies) cannot be loaded."
  (or (find-package :woo-showcase)
      (let ((dir (asdf:system-relative-pathname :woo "showcase/")))
        (pushnew dir asdf:*central-registry* :test #'equal)
        (handler-case
            (handler-bind ((warning #'muffle-warning))
              (let ((*standard-output* (make-broadcast-stream))
                    (*error-output* (make-broadcast-stream)))
                (asdf:load-system :woo-showcase))
              t)
          (error (e)
            (format *error-output* "~&woo-showcase did not load: ~A~%" e)
            nil)))))

(defun showcase-call (name &rest args)
  (apply (symbol-function (find-symbol name :woo-showcase)) args))

(defun js-member-keys (text objects)
  "Property names read as OBJECT.<name> in TEXT for each variable in OBJECTS."
  (let ((keys '()))
    (dolist (var objects (sort (remove-duplicates keys :test #'string=) #'string<))
      (let ((needle (concatenate 'string var ".")))
        (loop for pos = (search needle text) then (search needle text :start2 (1+ pos))
              while pos
              do (let ((before (and (plusp pos) (char text (1- pos))))
                       (start (+ pos (length needle))))
                   (unless (and before (or (alphanumericp before) (find before "_$.")))
                     (let ((end (or (position-if-not (lambda (c) (or (alphanumericp c)
                                                                     (find c "_$")))
                                                     text :start start)
                                    (length text))))
                       (when (> end start)
                         (push (subseq text start end) keys))))))))))

(deftest showcase-benchmarks-json-shape
  (if (not (showcase-loaded-p))
      (skip "woo-showcase does not load in this image; /api/benchmarks JSON not checked")
      (let* ((json (showcase-call "BENCHMARKS-JSON"))
             (parsed (uiop:symbol-call :jonathan :parse json :as :hash-table))
             (charts-js (uiop:read-file-string
                         (asdf:system-relative-pathname :woo "showcase/static/js/charts.js")))
             (js-keys (js-member-keys charts-js '("d" "a" "b"))))
        (testing "exact encoding of the first two entries"
          (ok (eql 0 (search "[{\"name\":\"Woo (4 workers)\",\"language\":\"Common Lisp\",\"requestsPerSec\":110528,\"latencyAvgMs\":1.12,\"latencyMaxMs\":21.69,\"highlight\":true},{\"name\":\"Go (GOMAXPROCS=4)\",\"language\":\"Go\",\"requestsPerSec\":106036,\"latencyAvgMs\":1.26,\"latencyMaxMs\":62.68,\"highlight\":false},"
                             json))
              "camelCase keys and real booleans")
          (ok (not (search "[]" json)) "no NIL encoded as an empty array")
          (ok (notany #'upper-case-p
                      (remove-if-not #'alpha-char-p
                                     (subseq json 0 (search ":" json))))
              "keys are not upcased"))
        (testing "every key charts.js reads is in every entry"
          (ok (equal js-keys '("highlight" "language" "latencyAvgMs" "latencyMaxMs"
                               "name" "requestsPerSec"))
              (format nil "charts.js reads ~{~A~^, ~}" js-keys))
          (ok (= (length parsed) 10))
          (dolist (entry parsed)
            (dolist (key js-keys)
              (multiple-value-bind (value present) (gethash key entry)
                (declare (ignore value))
                (ok present (format nil "~A has ~A" (gethash "name" entry) key))))
            (ok (numberp (gethash "requestsPerSec" entry)))
            (ok (numberp (gethash "latencyAvgMs" entry)))
            (ok (stringp (gethash "name" entry)))))
        (testing "highlight is true or false, never []"
          (ok (= (count-if (lambda (e) (eq (gethash "highlight" e) t)) parsed) 2))
          (ok (= (length (remove-duplicates
                          (loop for pos = (search "\"highlight\":" json)
                                  then (search "\"highlight\":" json :start2 (1+ pos))
                                while pos
                                collect (let ((v (+ pos 12)))
                                          (subseq json v (position-if (lambda (c) (find c ",}")) json :start v))))
                          :test #'string=))
                 2)
              "only the literals true and false")))))
