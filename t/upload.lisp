(in-package :cl-user)
(defpackage woo-test.upload
  (:use :cl :rove))
(in-package :woo-test.upload)

(deftest declared-body-limit-is-rejected
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (called nil))
    (clack.test:testing-app "Reject an oversized declared request body"
        (lambda (env)
          (declare (ignore env))
          (setf called t)
          '(200 nil ("unexpected")))
      (let ((response
              (woo-test::raw-exchange
               clack.test:*clack-test-port*
               (woo-test::crlf-lines
                "POST /upload HTTP/1.1"
                "Host: localhost"
                "Content-Length: 1073741825"
                "Connection: close"
                ""))))
        (ok (and (search "413 Request Entity Too Large" response)
                 (not called)))))))

(deftest fragmented-fixed-body-preserves-next-request
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (seen nil))
    (clack.test:testing-app "Keep fragmented fixed bodies aligned"
        (lambda (env)
          (push (getf env :path-info) seen)
          '(200 (:content-type "text/plain") ("ok")))
      (let ((response
              (woo-test::raw-exchange
               clack.test:*clack-test-port*
               (woo-test::crlf-lines
                "POST /body HTTP/1.1" "Host: localhost" "Content-Length: 4" ""
                )
               "ab"
               (concatenate 'string "cd"
                            (woo-test::crlf-lines
                             "GET /next HTTP/1.1" "Host: localhost" "Connection: close" "")))))
        (ok (and (search "HTTP/1.1 200 OK" response)
                 (equal (reverse seen) '("/body" "/next"))))))))

(deftest spilled-body-files-are-removed-after-response-and-abort
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (dir smart-buffer::*temporary-directory*)
        (old-memory smart-buffer::*default-memory-limit*)
        (old-disk smart-buffer::*default-disk-limit*)
        (saw-spill nil))
    (unwind-protect
         (progn
           ;; These are global defaults because the request parser runs in a
           ;; worker thread; lexical bindings in this test thread do not cross
           ;; that boundary.
           (setf (symbol-value 'smart-buffer::*default-memory-limit*) 1
                 (symbol-value 'smart-buffer::*default-disk-limit*) 64)
           (clack.test:testing-app "Remove spilled request body after response"
               (lambda (env)
                 (let ((stream (getf env :raw-body)))
                   (setf saw-spill (and (streamp stream)
                                        (probe-file (pathname stream)))))
                 '(200 nil ("ok")))
             (woo-test::raw-exchange
              clack.test:*clack-test-port*
              (woo-test::crlf-lines "POST /spill HTTP/1.1" "Host: localhost"
                                    "Content-Length: 4" "")
              "data"))
           (ok saw-spill)
           (ok (null (directory (merge-pathnames "*" dir))))
           (clack.test:testing-app "Remove spilled request body after abort"
               (lambda (env)
                 (declare (ignore env))
                 '(200 nil ("unexpected")))
             (woo-test::raw-exchange
              clack.test:*clack-test-port*
              (woo-test::crlf-lines "POST /abort HTTP/1.1" "Host: localhost"
                                    "Content-Length: 8" "")
              "data"))
           (ok (null (directory (merge-pathnames "*" dir)))))
      (setf (symbol-value 'smart-buffer::*default-memory-limit*) old-memory
            (symbol-value 'smart-buffer::*default-disk-limit*) old-disk))))
