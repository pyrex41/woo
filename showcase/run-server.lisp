;; Use local woo, not quicklisp version (local has WebSocket exports)
(let* ((here (uiop:pathname-directory-pathname *load-truename*))
       (woo-root (uiop:pathname-parent-directory-pathname here)))
  (push woo-root asdf:*central-registry*)
  (push here asdf:*central-registry*))

(ql:quickload :woo-showcase :silent t)

(format t "~%Starting Woo Showcase server...~%")
(woo-showcase:start :port 5000 :worker-num 1)
