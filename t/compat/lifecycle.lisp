(in-package :woo.compat.tests)
(defun open-fd-count ()
  (let* ((pid (sb-posix:getpid))
         (output (uiop:run-program (list "lsof" "-a" "-p" (write-to-string pid) "-d" "0-99999" "-Ff")
                                   :output :string :error-output :string)))
    (count-if (lambda (line) (and (plusp (length line)) (char= (char line 0) #\f)))
              (uiop:split-string output :separator '(#\Newline)))))

(deftest managed-clack-stop-repeatedly
  ;; This is a required lifecycle gate, not a smoke-test skip.
  (let ((fds-before (open-fd-count)))
  (dotimes (i 100)
    (with-managed (handler port (lambda (env) (declare (ignore env)) '(200 nil ("ok"))))
      (ok (string= (get-body port) "ok"))
      (clack:stop handler)
      (let ((state (woo.compat:server-state handler)))
        (ok (eq (getf state :state) :stopped))
        (ok (every #'zerop (mapcar (lambda (k) (getf state k))
                                   '(:queued :running :requests :connections :input-bytes :output-bytes :live-workers)))))))

  (ok (<= (open-fd-count) (+ fds-before 2)) "file descriptors return to baseline")))

(deftest occupied-port-startup-cleans-workers
  (let ((listener (usocket:socket-listen "127.0.0.1" 0 :reuse-address t)))
    (unwind-protect
         (ok (signals (woo.compat:clackup #'contract-app :address "127.0.0.1"
                                         :port (usocket:get-local-port listener) :startup-timeout 1)))
      (usocket:socket-close listener))))

(deftest shutdown-reports-uncooperative-worker
  (let ((entered (bt2:make-semaphore :count 0)) (release (bt2:make-semaphore :count 0))
        (client nil))
    (with-managed (handler port
                   (lambda (env) (declare (ignore env))
                     (bt2:signal-semaphore entered) (bt2:wait-on-semaphore release)
                     '(200 nil ("late"))) :application-workers 1 :drain-timeout 0.05 :cleanup-timeout 0.1)
      (unwind-protect
           (progn
             (setf client (bt2:make-thread (lambda () (ignore-errors (get-body port)))))
             (ok (bt2:wait-on-semaphore entered :timeout 5))
             (ok (signals (clack:stop handler) 'woo.compat:shutdown-timeout))
             (ok (= (getf (woo.compat:server-state handler) :live-workers) 1)))
        (bt2:signal-semaphore release) (when client (bt2:join-thread client))
        (await-state handler (lambda (s) (zerop (getf s :live-workers))))))))
