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

(let* ((port (parse-integer (or (uiop:getenv "WOO_HEGEL_PORT")
                               (error "WOO_HEGEL_PORT is required"))))
       (tls-port (1+ port))
       (nonce (or (uiop:getenv "WOO_HEGEL_READY_NONCE") ""))
       (static-file (or (uiop:getenv "WOO_LEGACY_STATIC_FILE")
                        (error "WOO_LEGACY_STATIC_FILE is required")))
       (cert-root (uiop:ensure-directory-pathname
                   (or (uiop:getenv "WOO_COMPAT_CERT_ROOT") "t/certs/")))
       (app (lambda (env)
              (let ((path (getf env :path-info)))
                (cond
                  ((string= path "/.woo-test-ready")
                   (list 200 '(:content-type "text/plain") (list nonce)))
                  ((and (stringp path) (uiop:string-prefix-p "/status/" path))
                   (let ((code (parse-integer path :start 8 :junk-allowed nil)))
                     (list code '(:content-type "text/plain") (list (write-to-string code)))))
                  ((string= path "/static/fixture.txt")
                   (list 200 '(:content-type "text/plain") (pathname static-file)))
                  ((string= path "/upload")
                   (list 200 '(:content-type "application/octet-stream")
                         (list (lack.request:request-content
                                (lack.request:make-request env)))))
                  ((string= path "/slow") (sleep 0.1) '(200 nil ("slow")))
                  (t '(404 (:content-type "text/plain") ("not found")))))))
       (plain nil) (tls nil))
  (unwind-protect
       (progn
         (setf tls (clack:clackup app :server :woo :use-thread t
                                  :address "127.0.0.1" :port tls-port
                                  :max-request-body-bytes 65536
                                  :handle-signals nil
                                  :ssl-key-file (merge-pathnames "localhost.key" cert-root)
                                  :ssl-cert-file (merge-pathnames "localhost.crt" cert-root)))
         (setf plain (clack:clackup app :server :woo :use-thread t
                                    :address "127.0.0.1" :port port
                                    :max-request-body-bytes 65536
                                    :handle-signals nil))
         (loop (sleep 1)))
    (when plain (ignore-errors (clack:stop plain)))
    (when tls (ignore-errors (clack:stop tls)))))
