(defpackage woo.compat.tests (:use :cl :rove))
(in-package :woo.compat.tests)

(defun free-port ()
  (let ((s (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect (usocket:get-local-port s) (usocket:socket-close s))))
(defmacro with-managed ((handler port app &rest options) &body body)
  `(let* ((,port (free-port))
          (,handler (woo.compat:clackup ,app :address "127.0.0.1" :port ,port ,@options)))
     (unwind-protect (progn ,@body) (clack:stop ,handler))))
(defun get-body (port &optional (path "/"))
  (dex:get (format nil "http://127.0.0.1:~D~A" port path) :keep-alive nil :force-string t :read-timeout 5 :connect-timeout 5))
(defun await-state (handler predicate)
  (loop with deadline = (+ (woo.compat::now) 5)
        for state = (woo.compat:server-state handler)
        when (funcall predicate state) return state
        when (> (woo.compat::now) deadline) do (error "State did not converge: ~S" state)
        do (sleep 0.01)))
(defun env (&optional (cookie ""))
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "accept" h) "*/*" (gethash "cookie" h) cookie)
    (list :request-method :get :script-name "" :path-info "/" :request-uri "/"
          :server-name "localhost" :server-port 80 :url-scheme "http"
          :server-protocol :http/1.1 :headers h)))
(defun contract-app (env)
  (let ((path (getf env :path-info)))
    (cond
      ((string= path "/stream")
       (lambda (respond)
         (let ((write (funcall respond '(200 (:content-type "text/plain")))))
           (funcall write "こんにちは λ") (funcall write nil :close t))))
      ((string= path "/empty") '(204 (:content-length 123) ("forbidden")))
      ((string= path "/cookies") '(200 (:set-cookie "a=1" :set-cookie "b=2") ("ok")))
      ((string= path "/echo")
       (list 200 '(:content-type "application/octet-stream")
             (lack.request:request-content (lack.request:make-request env))))
      (t '(200 (:content-type "text/plain") ("こんにちは λ"))))))

(deftest contract-over-real-http1
  (with-managed (handler port #'contract-app)
    (ok (string= (get-body port) "こんにちは λ"))
    (ok (string= (get-body port "/stream") "こんにちは λ"))
    (multiple-value-bind (body status) (get-body port "/empty")
      (ok (= status 204)) (ok (zerop (length body))))
    (multiple-value-bind (body status headers) (dex:request (format nil "http://127.0.0.1:~D/" port) :method :head :keep-alive nil)
      (declare (ignore body)) (ok (= status 200))
      (ok (equal (gethash "content-length" headers) "18")))
    (let ((body (make-array 257 :element-type '(unsigned-byte 8))))
      (dotimes (i (length body)) (setf (aref body i) (mod i 251)))
      (ok (equalp body (dex:post (format nil "http://127.0.0.1:~D/echo" port)
                                :content body :headers '(("content-type" . "application/octet-stream"))
                                :force-binary t :keep-alive nil))))
    (ok (zerop (getf (await-state handler (lambda (s) (zerop (getf s :requests)))) :input-bytes)))))

(deftest hunchentoot-baseline
  (let* ((port (free-port))
         (acceptor (make-instance 'clack.handler.hunchentoot::clack-acceptor
                                 :app #'contract-app :debug nil :address "127.0.0.1" :port port
                                 :access-log-destination nil)))
    (unwind-protect
         (progn (hunchentoot:start acceptor)
                (ok (string= (get-body port) "こんにちは λ")))
      (hunchentoot:stop acceptor))))

(deftest application-runs-off-network-loop
  (let ((entered (bt2:make-semaphore :count 0)) (release (bt2:make-semaphore :count 0)))
    (with-managed (handler port
                   (lambda (env)
                     (when (string= (getf env :path-info) "/slow")
                       (bt2:signal-semaphore entered) (bt2:wait-on-semaphore release :timeout 5))
                     '(200 nil ("ok"))) :application-workers 2)
      (let ((client (bt2:make-thread (lambda () (get-body port "/slow")))))
        (unwind-protect
             (progn (ok (bt2:wait-on-semaphore entered :timeout 5))
                    (ok (string= (get-body port) "ok")))
          (bt2:signal-semaphore release) (bt2:join-thread client))))))

(deftest streaming-response-exactly-once
  (let ((second :unknown) (late :unknown))
    (with-managed (handler port
                   (lambda (env) (declare (ignore env))
                     (lambda (respond)
                       (let ((writer (funcall respond '(200 nil))))
                         (setf second (funcall respond '(201 nil ("bad"))))
                         (funcall writer "ok" :close t)
                         (setf late (funcall writer "bad"))))))
      (ok (string= (get-body port) "ok"))
      (ok (null second)) (ok (null late)))))

(deftest request-body-limit-and-disconnect-cancellation
  (let ((observed nil) (entered (bt2:make-semaphore :count 0)) (release (bt2:make-semaphore :count 0)))
    (with-managed (handler port
                   (lambda (env)
                     (setf observed env)
                     (bt2:signal-semaphore entered)
                     (bt2:wait-on-semaphore release :timeout 5)
                     '(200 (:content-type "text/plain") ("late"))) :application-workers 1 :max-request-body-bytes 8)
      (let ((client (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8))))
        (unwind-protect
             (progn
               (write-sequence (trivial-utf-8:string-to-utf-8-bytes
                                (format nil "GET / HTTP/1.1~C~CHost: localhost~C~C~C~C"
                                        #\Return #\Linefeed #\Return #\Linefeed #\Return #\Linefeed))
                               (usocket:socket-stream client))
               (force-output (usocket:socket-stream client))
               (ok (bt2:wait-on-semaphore entered :timeout 5))
               (usocket:socket-close client)
               (await-state handler (lambda (state) (zerop (getf state :connections))))
               (ok (funcall (getf observed :woo.request-cancelled-p))))
          (ignore-errors (usocket:socket-close client)) (bt2:signal-semaphore release))))))

(deftest headers-and-framing-rejected-before-write
  (ok (signals (woo.compat::response-headers (list :x (format nil "good~C~Cbad" #\Return #\Linefeed)))))
  (with-managed (handler port (lambda (env) (declare (ignore env)) '(200 (:content-length 1) ("too long"))))
    (ok (signals (get-body port) 'dex:http-request-internal-server-error))))

(deftest body-and-queue-admission
  (with-managed (handler port #'contract-app :max-request-body-bytes 8)
    (ok (signals (dex:post (format nil "http://127.0.0.1:~D/echo" port)
                           :content "123456789" :keep-alive nil :read-timeout 5)
                 'dex:http-request-payload-too-large))
    (await-state handler (lambda (s) (zerop (getf s :connections)))))
  (let ((entered (bt2:make-semaphore :count 0)) (release (bt2:make-semaphore :count 0)) (clients nil))
    (with-managed (handler port
                   (lambda (env) (declare (ignore env))
                     (bt2:signal-semaphore entered) (bt2:wait-on-semaphore release :timeout 5)
                     '(200 (:content-type "text/plain") ("ok")))
                   :application-workers 1 :max-pending-requests 1)
      (unwind-protect
           (progn
             (push (bt2:make-thread (lambda () (ignore-errors (get-body port)))) clients)
             (ok (bt2:wait-on-semaphore entered :timeout 5))
             (push (bt2:make-thread (lambda () (ignore-errors (get-body port)))) clients)
             (await-state handler (lambda (s) (= (getf s :queued) 1)))
             (ok (signals (get-body port) 'dex:http-request-service-unavailable)))
        (bt2:signal-semaphore release) (bt2:signal-semaphore release)
        (dolist (client clients) (ok (equal (bt2:join-thread client) "ok")))))))

(deftest completion-registration-after-cancellation
  (let* ((server (woo.compat::make-managed-server))
         (request (woo.compat::make-managed-request :server server)) (calls 0))
    (woo.compat::cancel-request request)
    (woo.compat::on-completion request (lambda (r) (declare (ignore r)) (incf calls)))
    (woo.compat::cancel-request request)
    (ok (= calls 1))))

(deftest expired-dispatch-does-not-run-later
  (let* ((server (woo.compat::make-managed-server :cleanup-timeout 0.05))
         (socket (woo.ev.socket::%make-socket :fd -1 :last-activity 0d0
                   :watchers (make-array 3 :element-type 'cffi:foreign-pointer
                                        :initial-element (cffi:null-pointer))))
         (queued nil) (called nil)
         (connection (make-instance 'woo.compat::connection :server server :socket socket
                       :owner nil :runner (lambda (thunk) (setf queued thunk) t))))
    (ok (signals (woo.compat:call-on-connection connection (lambda () (setf called t)))
                 'woo.compat:connection-closed))
    (funcall queued)
    (ok (null called))))

(deftest application-error-before-and-after-headers
  (dolist (app (list (lambda (env) (declare (ignore env)) (error "fixture"))
                    (lambda (env) (declare (ignore env))
                      (lambda (respond) (declare (ignore respond)) (error "fixture")))))
    (with-managed (handler port app)
      (ok (signals (get-body port) 'dex:http-request-internal-server-error))))
  (let ((observed nil))
    (with-managed (handler port
                   (woo.compat::wrap-backtrace
                    (lambda (env)
                      (setf observed env)
                      (lambda (respond)
                        (funcall (funcall respond '(200 nil)) "partial")
                        (error "fixture"))) :logger (lambda (entry) (declare (ignore entry)))))
      ;; Dexador may accept truncated chunking; cancellation is the server gate.
      (ignore-errors (get-body port))
      (await-state handler (lambda (s) (and (zerop (getf s :requests))
                                          (zerop (getf s :output-bytes)))))
      (ok (funcall (getf observed :woo.request-cancelled-p))))))

(deftest socket-budget-refusal-keeps-server-alive
  (with-managed (handler port #'contract-app :max-server-queue-bytes 1)
    (let ((client (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8))))
      (unwind-protect
           (progn
             (write-sequence woo.http2.constants:+connection-preface+ (usocket:socket-stream client))
             (force-output (usocket:socket-stream client))
             (ok (null (sb-ext:with-timeout 5 (read-byte (usocket:socket-stream client) nil nil)))
                 "HTTP/2 SETTINGS budget refusal closes the connection"))
        (usocket:socket-close client)))
    (let ((state (await-state handler (lambda (s) (zerop (getf s :connections))))))
      (ok (eq (getf state :state) :running))
      (ok (zerop (getf state :output-bytes))))
    (ok (signals (get-body port)))
    (ok (eq (getf (woo.compat:server-state handler) :state) :running))))
