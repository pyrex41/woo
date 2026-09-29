;;; Legacy :server :woo fixture for the bounded qualification lane.
(require :asdf)
(unless (find-package :ql)
  (let ((setup (or (uiop:getenv "WOO_QUICKLISP_SETUP")
                   (namestring (merge-pathnames "quicklisp/setup.lisp"
                                                (user-homedir-pathname))))))
    (unless (probe-file setup) (error "Quicklisp setup not found at ~A" setup))
    (load setup)))

(dolist (entry '("clack-9435762a8fc139edde8c682502a037332b0655f8/"
                 "lack-35d8b0ab38a5f3a8ce885f9b135152ce932a45e2/"
                 "websocket-driver-137e3132075996b4b21874dad50a02e28aacc64c/"
                 "hunchentoot-65a3a3cca7e6e91fb068efc2ef8069b5c78ee91f/"))
  (let ((root (uiop:getenv "WOO_COMPAT_DEPENDENCY_ROOT")))
    (unless root (error "WOO_COMPAT_DEPENDENCY_ROOT is required"))
    (let ((path (merge-pathnames entry (uiop:ensure-directory-pathname root))))
      (unless (probe-file path) (error "Missing pinned source ~A" path))
      (push path asdf:*central-registry*))))
(pushnew (truename #P"./") asdf:*central-registry* :test #'equal)
(ql:quickload :cffi :silent t)
(dolist (directory (uiop:split-string
                    (or (uiop:getenv "WOO_HEGEL_FOREIGN_LIB_DIRS") "")
                    :separator '(#\:)))
  (when (plusp (length directory))
    (push (pathname (concatenate 'string directory "/"))
          cffi:*foreign-library-directories*)))
(ql:quickload :clack :silent t)
(ql:quickload :lack-request :silent t)
(ql:quickload :clack-handler-woo :silent t)

(defparameter *legacy-memory-metric-count* 0)
(defun emit-legacy-memory-metric (label)
  (when (and (string= (or (uiop:getenv "WOO_LEGACY_MEMORY_DIAGNOSTIC") "") "1")
             (< *legacy-memory-metric-count* 64))
    (format t "LEGACY_METRIC sbcl pid=~A label=~A gc_run_time=~A gc_real_time=~A bytes_consED=~A bytes_between_gcs=~A~{ gen~A_gcs=~A gen~A_bytes=~A~}~%"
            (sb-unix:unix-getpid)
            label sb-ext:*gc-run-time* sb-ext:*gc-real-time*
            (sb-ext:get-bytes-consed) (sb-ext:bytes-consed-between-gcs)
            (loop for generation from 0 to 6
                  append (list generation
                               (sb-ext:generation-number-of-gcs generation)
                               generation
                               (sb-ext:generation-bytes-allocated generation))))
    (finish-output)
    (incf *legacy-memory-metric-count*)))

(let* ((port (parse-integer (or (uiop:getenv "WOO_HEGEL_PORT")
                               (error "WOO_HEGEL_PORT is required"))))
       (tls-port (1+ port))
       (nonce (or (uiop:getenv "WOO_HEGEL_READY_NONCE") ""))
       (static-file (or (uiop:getenv "WOO_LEGACY_STATIC_FILE")
                        (error "WOO_LEGACY_STATIC_FILE is required")))
       (cert-root (uiop:ensure-directory-pathname
                   (or (uiop:getenv "WOO_COMPAT_CERT_ROOT") "t/certs/")))
       (stop-result (uiop:getenv "WOO_LEGACY_STOP_RESULT"))
       (spool-root (uiop:ensure-directory-pathname
                    (or (uiop:getenv "WOO_LEGACY_SPOOL_ROOT")
                        (error "WOO_LEGACY_SPOOL_ROOT is required"))))
       (plain nil) (tls nil)
       (limit-body (lambda (socket)
                     (setf (woo.ev.socket::socket-body-memory-limit socket) 65536)))
       (app (lambda (env)
              (let ((path (getf env :path-info)))
                (cond
                  ((string= path "/.woo-test-ready")
                   (list 200 '(:content-type "text/plain") (list nonce)))
                  ((and (stringp path) (uiop:string-prefix-p "/status/" path))
                   (let ((code (parse-integer path :start 8 :junk-allowed nil)))
                     (if (= code 204)
                         (list code nil nil)
                         (list code '(:content-type "text/plain") (list (write-to-string code))))))
                  ((string= path "/static/fixture.txt")
                   (list 200 '(:content-type "text/plain") (pathname static-file)))
                  ((string= path "/upload")
                   (list 200 '(:content-type "application/octet-stream")
                         (lack.request:request-content
                          (lack.request:make-request env))))
                  ((string= path "/slow") (sleep 0.1) '(200 nil ("slow")))
                  ((and (string= path "/.woo-memory-full-gc")
                        (string= (or (uiop:getenv "WOO_LEGACY_MEMORY_DIAGNOSTIC") "") "1")
                        (string= (or (uiop:getenv "WOO_LEGACY_FULL_GC_DIAGNOSTIC") "") "1"))
                   (emit-legacy-memory-metric "full-gc-before")
                   (sb-ext:gc :full t)
                   (emit-legacy-memory-metric "full-gc-after")
                   '(204 nil nil))
                  ((string= path "/stream")
                   (list 200 '(:content-type "application/octet-stream")
                         (list (make-string (* 128 1024) :initial-element #\A))))
                  ((string= path "/stop")
                   (bt2:make-thread
                    (lambda ()
                      (sleep 0.05)
                      (handler-case
                          (progn (clack:stop plain) (clack:stop tls)
                                 (when stop-result
                                   (with-open-file (out stop-result :direction :output
                                                         :if-exists :supersede)
                                     (write-line "PASS" out)))
                                 (sb-ext:exit :code 0))
                        (error () (when stop-result
                                    (with-open-file (out stop-result :direction :output
                                                          :if-exists :supersede)
                                      (write-line "FAIL" out)))))))
                   '(202 (:content-length 0) nil))
                  (t '(404 (:content-type "text/plain") ("not found"))))))))
  (ensure-directories-exist spool-root)
  (setf smart-buffer::*temporary-directory* spool-root)
  (unwind-protect
       (progn
         (setf tls (clack:clackup app :server :woo :use-thread t
                                  :address "127.0.0.1" :port tls-port
                                  :on-connection limit-body
                                  :handle-signals nil
                                  :ssl-key-file (merge-pathnames "localhost.key" cert-root)
                                  :ssl-cert-file (merge-pathnames "localhost.crt" cert-root)))
         (setf plain (clack:clackup app :server :woo :use-thread t
                                    :address "127.0.0.1" :port port
                                    :on-connection limit-body
                                    :handle-signals nil))
         (emit-legacy-memory-metric "server-start")
         (loop for seconds from 1 do
           (sleep 1)
           (when (zerop (mod seconds 30))
             (emit-legacy-memory-metric "server-soak"))))
    (when plain (ignore-errors (clack:stop plain)))
    (when tls (ignore-errors (clack:stop tls)))))
