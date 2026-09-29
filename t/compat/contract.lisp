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
(defun get-body/status (port path)
  (handler-case
      (dex:get (format nil "http://127.0.0.1:~D~A" port path)
               :keep-alive nil :force-string t :read-timeout 5 :connect-timeout 5)
    (dex:http-request-failed (condition)
      (values (dex:response-body condition) (dex:response-status condition)))))
(defun await-state (handler predicate)
  (loop with deadline = (+ (woo.compat::now) 5)
        for state = (woo.compat:server-state handler)
        when (funcall predicate state) return state
        when (> (woo.compat::now) deadline) do (error "State did not converge: ~S" state)
        do (sleep 0.01)))
(defun await-closed-stream (stream)
  (loop repeat 500
        when (and stream (not (open-stream-p stream))) return t
        do (sleep 0.01)
        finally (return nil)))
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

(deftest managed-static-path-errors
  "Path failures are reported before response headers, including real EACCES."
  (let* ((root (merge-pathnames
                (format nil "woo-compat-static-~D/" (random 1000000000))
                (uiop:temporary-directory)))
         (file (merge-pathnames "body" root))
         (directory (merge-pathnames "directory/" root))
         (missing (merge-pathnames "missing" root)))
    (ensure-directories-exist directory)
    (with-open-file (stream file :direction :output :if-exists :supersede)
      (write-string "body" stream))
    (flet ((app (env)
             (let ((path (getf env :path-info)))
               (cond
                 ((string= path "/missing") (list 200 nil missing))
                 ((string= path "/directory") (list 200 nil directory))
                 ((string= path "/mismatch") (list 200 '(:content-length 1) file))
                 ((string= path "/denied") (list 200 nil file))
                 (t '(404 nil ()))))))
      (with-managed (handler port #'app)
        (multiple-value-bind (body status) (get-body/status port "/missing")
          (declare (ignore body)) (ok (= status 404)))
        (multiple-value-bind (body status) (get-body/status port "/directory")
          (declare (ignore body)) (ok (= status 403)))
        (let ((captured nil)
              (old-hook woo.compat::*pathname-body-prepared-hook*))
          (unwind-protect
               (progn
                 (setf woo.compat::*pathname-body-prepared-hook*
                       (lambda (body stream)
                         (when (equal body file) (setf captured stream))))
                 (multiple-value-bind (body status) (get-body/status port "/mismatch")
                   (declare (ignore body)) (ok (= status 500)))
                 ;; The response can reach the client before the worker's
                 ;; unwind-protect closes the prepared stream. Poll the
                 ;; ownership boundary instead of racing that cleanup.
                 (ok (await-closed-stream captured)))
            (setf woo.compat::*pathname-body-prepared-hook* old-hook)))
        ;; Root can bypass mode bits, so this assertion is intentionally only
        ;; run by a non-root test process. It exercises the actual open(2)
        ;; failure rather than relying on pathname preflight heuristics.
        (unless (zerop #+sbcl (sb-posix:getuid) #-sbcl 1)
          (ok (zerop (wsys:chmod (namestring file) 0)))
          (unwind-protect
               (multiple-value-bind (body status) (get-body/status port "/denied")
                 (declare (ignore body)) (ok (= status 403)))
            (wsys:chmod (namestring file) #o600))))
    (ignore-errors (delete-file file))
    (ignore-errors (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))

(deftest managed-static-uses-prepared-open-stream
  (let* ((root (merge-pathnames
                (format nil "woo-compat-identity-~D/" (random 1000000000))
                (uiop:temporary-directory)))
         (path (merge-pathnames "body" root))
         (backup (merge-pathnames "body.old" root))
         (replacement (merge-pathnames "body.new" root)))
    (ensure-directories-exist root)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string "original" s))
    (with-open-file (s replacement :direction :output :if-exists :supersede)
      (write-string "replacement" s))
    (let ((hook-ran nil)
          (allow-rename nil)
          (captured-stream nil)
          (old-hook woo.compat::*pathname-body-prepared-hook*))
      (unwind-protect
           (progn
             (setf woo.compat::*pathname-body-prepared-hook*
                   (lambda (body stream)
                     (setf captured-stream stream)
                     (when allow-rename
                       (setf hook-ran t))
                     (when (and allow-rename (probe-file body))
                       (rename-file body backup)
                       (rename-file replacement body))))
           (with-managed (handler port (lambda (env)
                                         (declare (ignore env))
                                         (list 200 nil path)))
             (multiple-value-bind (body status headers)
                 (dex:request (format nil "http://127.0.0.1:~D/" port)
                              :method :head :keep-alive nil :force-string t)
               (ok (= status 200))
               (ok (zerop (length body)))
               (ok (string= (gethash "content-length" headers) "8")))
             (setf allow-rename t)
             (ok (string= (get-body port) "original"))
             (ok hook-ran)
             (ok (await-closed-stream captured-stream)))
        (setf woo.compat::*pathname-body-prepared-hook* old-hook)
        (ignore-errors (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))))))

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

(deftest managed-producer-retains-body-after-disconnect
  (let* ((temporary-directory
           (uiop:ensure-directory-pathname
            (merge-pathnames
             (format nil "woo-managed-body-~36R/" (random (expt 36 8)))
             (uiop:temporary-directory))))
         (body (make-array 64 :element-type '(unsigned-byte 8)))
         (entered (bt2:make-semaphore :count 0))
         (allow-read (bt2:make-semaphore :count 0))
         (producer-done (bt2:make-semaphore :count 0))
         (observed-env nil)
         (observed-stream nil)
         (stream-open-before-read nil)
         (observed-bytes nil)
         (body-file nil))
    (dotimes (i (length body)) (setf (aref body i) (mod (+ i 17) 251)))
    (ensure-directories-exist temporary-directory)
    (unwind-protect
         (let ((memory-limit smart-buffer:*default-memory-limit*)
               (disk-limit smart-buffer:*default-disk-limit*)
               (default-directory smart-buffer::*temporary-directory*))
           (setf smart-buffer:*default-memory-limit* 1
                 smart-buffer:*default-disk-limit* 4096
                 smart-buffer::*temporary-directory* temporary-directory)
           (unwind-protect
                (with-managed
               (handler port
                (lambda (env)
                  (let ((raw-body (getf env :raw-body)))
                    (setf observed-env env
                          observed-stream raw-body
                          body-file (and (typep raw-body 'file-stream)
                                         (pathname raw-body)))
                    (bt2:signal-semaphore entered)
                    (bt2:wait-on-semaphore allow-read :timeout 5)
                    (setf stream-open-before-read (open-stream-p raw-body))
                    (let ((received (make-array (length body)
                                                :element-type '(unsigned-byte 8))))
                      (read-sequence received raw-body)
                      (setf observed-bytes received))
                    (bt2:signal-semaphore producer-done)
                    '(200 (:content-length 2) ("ok"))))
                :application-workers 1)
             (let ((client (usocket:socket-connect
                            "127.0.0.1" port
                            :element-type '(unsigned-byte 8))))
               (unwind-protect
                    (let ((stream (usocket:socket-stream client)))
                      (write-sequence
                       (trivial-utf-8:string-to-utf-8-bytes
                        (format nil "POST / HTTP/1.1~C~CHost: localhost~C~CContent-Length: ~D~C~C~C~C"
                                #\Return #\Newline #\Return #\Newline
                                (length body) #\Return #\Newline
                                #\Return #\Newline))
                       stream)
                      (write-sequence body stream)
                      (force-output stream)
                      (ok (bt2:wait-on-semaphore entered :timeout 5))
                      (usocket:socket-close client)
                      (await-state handler (lambda (state)
                                             (zerop (getf state :connections))))
                      (ok (funcall (getf observed-env :woo.request-cancelled-p)))
                      (bt2:signal-semaphore allow-read)
                      (ok (bt2:wait-on-semaphore producer-done :timeout 5)))
                 (ignore-errors (usocket:socket-close client))))
             (ok stream-open-before-read)
             (ok (equalp observed-bytes body))
             (ok body-file)
             (ok (equal (pathname-directory body-file)
                        (pathname-directory temporary-directory)))
             (ok (loop repeat 400
                       when (and observed-stream body-file
                                 (not (open-stream-p observed-stream))
                                 (not (probe-file body-file)))
                         return t
                       do (sleep 0.05)))
             (setf smart-buffer:*default-memory-limit* memory-limit
                   smart-buffer:*default-disk-limit* disk-limit
                   smart-buffer::*temporary-directory* default-directory)))
      (uiop:delete-directory-tree temporary-directory
                                   :validate t :if-does-not-exist :ignore)))))

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

(deftest application-worker-random-states-are-private
  (let ((lock (bt2:make-lock)) (states nil)
        (entered (bt2:make-semaphore :count 0)) (release (bt2:make-semaphore :count 0)))
    (with-managed (handler port
                   (lambda (env)
                     (declare (ignore env))
                     (bt2:with-lock-held (lock) (push *random-state* states))
                     (bt2:signal-semaphore entered)
                     (bt2:wait-on-semaphore release :timeout 5)
                     '(200 nil ("ok"))) :application-workers 4)
      (let ((clients (loop repeat 4 collect (bt2:make-thread (lambda () (get-body port))))))
        (unwind-protect
             (progn
               (dotimes (i 4) (ok (bt2:wait-on-semaphore entered :timeout 5)))
               (ok (= 4 (length (remove-duplicates states :test #'eq))))
               (ok (not (member *random-state* states :test #'eq))))
          (dotimes (i 4) (bt2:signal-semaphore release))
          (dolist (client clients) (sb-thread:join-thread (bt2:thread-native-thread client) :timeout 10)))))))
