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
        (dir (uiop:ensure-directory-pathname
              (merge-pathnames
               (format nil "woo-upload-~36R/" (random (expt 36 8)))
               (uiop:temporary-directory))))
        (old-directory smart-buffer::*temporary-directory*)
        (old-memory smart-buffer::*default-memory-limit*)
        (old-disk smart-buffer::*default-disk-limit*)
        (saw-spill nil)
        (keepalive-seen nil))
    (unwind-protect
         (progn
           ;; These are global defaults because the request parser runs in a
           ;; worker thread; lexical bindings in this test thread do not cross
           ;; that boundary.
           (setf (symbol-value 'smart-buffer::*default-memory-limit*) 1
                 (symbol-value 'smart-buffer::*default-disk-limit*) 64
                 (symbol-value 'smart-buffer::*temporary-directory*) dir)
           (ensure-directories-exist dir)
           (clack.test:testing-app "Remove spilled request body after response"
               (lambda (env)
                 (if (string= (getf env :path-info) "/next")
                     (progn (setf keepalive-seen t) '(200 nil ("next")))
                     (let ((stream (getf env :raw-body)))
                       (setf saw-spill (and (streamp stream)
                                            (probe-file (pathname stream))))
                       '(200 nil ("ok")))))
             (woo-test::raw-exchange
              clack.test:*clack-test-port*
              (woo-test::crlf-lines "POST /spill HTTP/1.1" "Host: localhost"
                                    "Content-Length: 4" "")
              (concatenate 'string "data"
                           (woo-test::crlf-lines "GET /next HTTP/1.1"
                                                 "Host: localhost"
                                                 "Connection: close" ""))))
           (ok saw-spill)
           (ok keepalive-seen)
           (ok (null (directory (merge-pathnames "*" dir))))
           (let ((response (make-string (* 1024 1024) :initial-element #\x))
                 (abort-seen nil)
                 (abort-body-path nil))
             (clack.test:testing-app "Remove spilled request body after abort"
                 (lambda (env)
                   (if (string= (getf env :path-info) "/health")
                       '(200 nil ("healthy"))
                       (progn
                         (let ((stream (getf env :raw-body)))
                           (setf abort-body-path (and (streamp stream)
                                                      (probe-file (pathname stream)))
                                 abort-seen (not (null abort-body-path))))
                         (sleep 0.2)
                         (list 200 nil (list response)))))
               (let ((client (usocket:socket-connect
                              "127.0.0.1" clack.test:*clack-test-port*
                              :element-type '(unsigned-byte 8))))
                 (unwind-protect
                      (let ((stream (usocket:socket-stream client)))
                        (write-sequence
                         (trivial-utf-8:string-to-utf-8-bytes
                          (concatenate 'string
                                       (woo-test::crlf-lines
                                        "POST /abort HTTP/1.1" "Host: localhost"
                                        "Content-Length: 4" "")
                                       "data"))
                         stream)
                        (force-output stream)
                        (loop repeat 100
                              until abort-seen
                              do (sleep 0.01))
                        (ok abort-seen)
                        (ok abort-body-path))
                   (usocket:socket-close client)))
               (loop repeat 100
                     while (directory (merge-pathnames "*" dir))
                     do (sleep 0.01))
               (ok (null (directory (merge-pathnames "*" dir))))
               (ok (search "HTTP/1.1 200 OK"
                           (woo-test::raw-exchange
                            clack.test:*clack-test-port*
                            (woo-test::crlf-lines "GET /health HTTP/1.1"
                                                  "Host: localhost" "Connection: close" "")))))
           (ok (null (directory (merge-pathnames "*" dir)))))
      (setf (symbol-value 'smart-buffer::*default-memory-limit*) old-memory
            (symbol-value 'smart-buffer::*default-disk-limit*) old-disk
            (symbol-value 'smart-buffer::*temporary-directory*) old-directory)
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))
