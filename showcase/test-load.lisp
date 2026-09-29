;; Use local woo, not quicklisp version (local has WebSocket exports)
(let* ((here (uiop:pathname-directory-pathname *load-truename*))
       (woo-root (uiop:pathname-parent-directory-pathname here)))
  (push woo-root asdf:*central-registry*)
  (push here asdf:*central-registry*))
(handler-case
  (progn
    (ql:quickload :woo-showcase :silent t)
    (format t "~%SUCCESS: woo-showcase loaded successfully!~%")
    (sb-ext:exit :code 0))
  (error (e)
    (format t "~%ERROR: ~A~%" e)
    (sb-ext:exit :code 1)))
