(in-package :woo.compat.tests)
(defun session-service-check (store)
  (let* ((sid (make-string 40 :initial-element #\b))
         (app (lack:builder (:woo-session :store store)
                (lambda (e)
                  (sleep 0.001)
                  (incf (gethash :count (getf e :lack.session) 0))
                  (lambda (respond) (funcall respond '(200 nil ("ok"))))))))
    (unwind-protect
         (let ((threads (loop repeat 12 collect
                         (bt2:make-thread
                          (lambda () (funcall (funcall app (env (format nil "lack.session=~A" sid))) #'identity))))))
           (mapc #'bt2:join-thread threads)
           (ok (= (gethash :count (lack.session.store:fetch-session store sid)) 12)))
      (lack.session.store:remove-session store sid))))

(deftest sqlite-session-and-pool
  (let* ((path (merge-pathnames (format nil "woo-compat-~D.sqlite" (random 1000000000)) (uiop:temporary-directory)))
         (conn (dbi:connect :sqlite3 :database-name path)))
    (unwind-protect
         (progn
           (dbi:do-sql conn "CREATE TABLE sessions (id TEXT PRIMARY KEY, session_data TEXT)")
           (session-service-check (lack.session.store.dbi:make-dbi-store :connector (lambda () conn)))
           (let ((app (lack:builder
                        (:dbpool :woo-compat-test :connect-args (list :sqlite3 :database-name path)
                         :pool-args '(:max-open-count 2 :max-idle-count 0 :timeout 1000))
                        (lambda (env) (declare (ignore env))
                          (lack.middleware.dbpool:with-connection (c :woo-compat-test)
                            (let ((row (dbi:fetch (dbi:execute (dbi:prepare c "SELECT 42 AS answer")))))
                              (list 200 nil (list (write-to-string (getf row :|answer|))))))))))
             (with-managed (handler port app) (ok (string= (get-body port) "42")))))
      (dbi:disconnect conn) (when (probe-file path) (delete-file path)))))

(deftest redis-session
  ;; The runner owns a private loopback Redis, with no persistence.
  (let ((port (uiop:getenv "WOO_COMPAT_REDIS_PORT")))
    (unless port (error "WOO_COMPAT_REDIS_PORT is required; Redis qualification cannot be skipped"))
    (let* ((store (lack.session.store.redis:make-redis-store :port (parse-integer port)
                  :namespace (format nil "woo-compat-~D" (random 1000000000)))))
      (unwind-protect (session-service-check store)
        (let ((redis::*connection* (lack.middleware.session.store.redis::redis-store-connection store)))
          (redis:disconnect))))))
