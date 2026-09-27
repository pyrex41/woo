;;; Live Woo fixture for the Hegel Go tests. Run from the repository root.
(require :asdf)
(unless (find-package :ql)
  (let ((setup (or (uiop:getenv "WOO_QUICKLISP_SETUP")
                   (namestring (merge-pathnames "quicklisp/setup.lisp"
                                                (user-homedir-pathname))))))
    (unless (probe-file setup)
      (error "Quicklisp setup not found at ~A" setup))
    (load setup)))

(pushnew (truename #P"./") asdf:*central-registry* :test #'equal)
(ql:quickload :cffi :silent t)
(dolist (directory (uiop:split-string
                    (or (uiop:getenv "WOO_HEGEL_FOREIGN_LIB_DIRS") "")
                    :separator '(#\:)))
  (when (plusp (length directory))
    (push (pathname (concatenate 'string directory "/"))
          cffi:*foreign-library-directories*)))
(ql:quickload :woo :silent t)
(ql:quickload :lack-request :silent t)

(let ((port (parse-integer (or (uiop:getenv "WOO_HEGEL_PORT")
                               (error "WOO_HEGEL_PORT is required"))))
      (ready-nonce (uiop:getenv "WOO_HEGEL_READY_NONCE")))
  (woo:run (lambda (env)
             (let ((path (getf env :path-info)))
               (cond
                 ((and ready-nonce (string= path "/.woo-test-ready"))
                  `(200 (:content-type "text/plain") (,ready-nonce)))
                 ((and (string= path "/ws") (woo:websocket-p env))
                  (let* ((socket (getf env :clack.io))
                         (key (gethash "sec-websocket-key" (getf env :headers))))
                    (woo:write-websocket-upgrade-response
                     socket (woo:compute-accept-key key))
                    (woo:setup-websocket
                     socket :on-message
                     (lambda (opcode payload)
                       (when (= opcode woo.websocket:+opcode-binary+)
                         (woo:send-binary-frame socket payload))))
                    nil))
                 ;; A fixed response larger than the initial connection window
                 ;; lets the raw client independently control both send windows.
                 ((string= path "/flow-body")
                  (let ((body (make-array 70000 :element-type '(unsigned-byte 8))))
                    (dotimes (i (length body))
                      (setf (aref body i) (mod i 251)))
                    `(200 (:content-type "application/octet-stream") ,body)))
                 ((string= path "/large-body")
                  `(200 (:content-type "text/plain")
                        ,(make-string (1+ (* 8 1024 1024))
                                      :initial-element #\a)))
                 ((string= path "/body")
                  `(200 (:content-type "application/octet-stream")
                        ,(lack.request:request-content
                          (lack.request:make-request env))))
                 ((and (stringp path) (search "/echo/" path :end2 (min 6 (length path))))
                  `(200 (:content-type "text/plain") (,path)))
                 (t '(404 (:content-type "text/plain") ("not found"))))))
           :address "127.0.0.1" :port port :debug nil :worker-num nil))
