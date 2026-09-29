;;; Live TLS fixture for tls_live_test.go.
;;; It exposes one plain readiness listener and two independent TLS listeners.
;;; This fixture needs only Woo itself; the managed Lack dependency bundle is
;;; deliberately outside this protocol qualification lane.
(require :asdf)
(unless (find-package :ql)
  (load (or (uiop:getenv "WOO_QUICKLISP_SETUP")
            (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))))
(pushnew (truename #P"./") asdf:*central-registry* :test #'equal)
(ql:quickload :cffi :silent t)
(dolist (directory (uiop:split-string
                    (or (uiop:getenv "WOO_HEGEL_FOREIGN_LIB_DIRS") "")
                    :separator '(#\:)))
  (when (plusp (length directory))
    (push (pathname (concatenate 'string directory "/"))
          cffi:*foreign-library-directories*)))
(ql:quickload :woo :silent t)

(defparameter *tls-live-ready* 0)
(defparameter *tls-live-ready-lock* (bt2:make-lock :name "tls-live-ready"))
(defparameter *tls-live-failure* nil)
(defparameter *tls-live-stop-requested* nil)
(defparameter *tls-live-threads* nil)

(defun tls-live-env (name)
  (or (uiop:getenv name) (error "~A is required" name)))

(defun tls-live-ready-p ()
  (bt2:with-lock-held (*tls-live-ready-lock*)
    (= *tls-live-ready* 2)))

(defun tls-live-note-ready ()
  (bt2:with-lock-held (*tls-live-ready-lock*)
    (incf *tls-live-ready*)))

(defun tls-live-file-descriptor-count ()
  (or (handler-case
          (let ((entries (directory #P"/proc/self/fd/*")))
            (and entries (length entries)))
        (error () nil))
      (handler-case
          (let* ((pid #+sbcl (write-to-string (sb-posix:getpid))
                     #-sbcl (error "process id is unavailable"))
                 (output (uiop:run-program
                          (list "lsof" "-p" pid "-Fn")
                          :output :string :ignore-error-status t))
                 (count (count #\Newline output)))
            (and (plusp count) count))
        (error () -1))))

(defun tls-live-context-count ()
  (handler-case
      (hash-table-count woo.ssl.alpn::*alpn-ctx-args*)
    (error () -1)))

(let* ((port (parse-integer (tls-live-env "WOO_HEGEL_PORT")))
       (nonce (tls-live-env "WOO_HEGEL_READY_NONCE"))
       (cert-a (pathname (tls-live-env "WOO_TLS_CERT_A")))
       (key-a (pathname (tls-live-env "WOO_TLS_KEY_A")))
       (cert-b (pathname (tls-live-env "WOO_TLS_CERT_B")))
       (key-b (pathname (tls-live-env "WOO_TLS_KEY_B")))
       (static-file (pathname (tls-live-env "WOO_TLS_STATIC_FILE")))
       (bad-key (uiop:getenv "WOO_TLS_BAD_KEY"))
       (final-counters (uiop:getenv "WOO_TLS_FINAL_COUNTERS"))
       (body-app
         (lambda (env)
           (let ((path (getf env :path-info)))
             (cond
               ((and (string= path "/.woo-test-ready") (tls-live-ready-p))
                `(200 (:content-type "text/plain") (,nonce)))
               ((string= path "/large-static")
                `(200 (:content-type "application/octet-stream") ,static-file))
               ((string= path "/counters")
                `(200 (:content-type "text/plain")
                      (,(format nil "fd=~D ctx=~D"
                                (tls-live-file-descriptor-count)
                                (tls-live-context-count)))))
               ((string= path "/stop")
                (setf *tls-live-stop-requested* t)
                (bt2:make-thread
                 (lambda ()
                   (sleep 0.05)
                   (dolist (thread *tls-live-threads*)
                     (woo:stop-gracefully thread)))
                 :name "woo-tls-stop-request")
                '(200 (:content-type "text/plain") ("stopping")))
               (t '(404 (:content-type "text/plain") ("not found")))))))
       (plain nil) (tls-a nil) (tls-b nil))
  (labels ((start-tls (cert key protocols tls-port)
             (let ((thread
                     (bt2:make-thread
              (lambda ()
                (handler-case
                    (let ((woo.ssl:*alpn-protocols* protocols))
                      (woo:run body-app :address "127.0.0.1" :port tls-port
                               :debug nil :worker-num nil
                               :ssl-cert-file cert :ssl-key-file key
                               :handle-signals nil
                               :on-ready (lambda (control)
                                           (declare (ignore control))
                                           (tls-live-note-ready))))
                  (error (condition)
                    (setf *tls-live-failure* condition)
                    (format *error-output* "TLS fixture startup failed: ~A~%"
                            condition)
                    (finish-output *error-output*))))
              :name (format nil "woo-tls-~D" tls-port))))
               (push thread *tls-live-threads*)
               thread)))
    (unwind-protect
         (progn
           ;; Start TLS first.  Plain readiness is withheld until both
           ;; listener contexts have completed their bind and ALPN setup.
           (setf tls-a (start-tls cert-a (or bad-key key-a)
                                  '("h2" "http/1.1") (1+ port)))
           (setf tls-b (start-tls cert-b key-b '("http/1.1") (+ port 2)))
           (setf plain
                 (bt2:make-thread
                  (lambda ()
                    (handler-case
                        (woo:run body-app :address "127.0.0.1" :port port
                                 :debug nil :worker-num nil :handle-signals nil)
                      (error (condition)
                        (setf *tls-live-failure* condition)
                        (format *error-output* "plain fixture startup failed: ~A~%"
                                condition)
                        (finish-output *error-output*))))
                  :name "woo-tls-plain"))
           (push plain *tls-live-threads*)
           (loop while (or (not *tls-live-stop-requested*)
                           (some #'bt2:thread-alive-p *tls-live-threads*)) do
             (when *tls-live-failure*
               (error "TLS fixture startup failed: ~A" *tls-live-failure*))
             (sleep 0.1))
           (when final-counters
             (with-open-file (stream final-counters :direction :output
                                     :if-exists :supersede :if-does-not-exist :create)
               (format stream "fd=~D ctx=~D~%"
                       (tls-live-file-descriptor-count)
                       (tls-live-context-count)))))
      ;; Startup failures are handled by the parent process group.  Normal
      ;; shutdown reaches this point only after every listener thread has
      ;; released its event loop and SSL context.
      (declare (ignore plain tls-a tls-b)))))
