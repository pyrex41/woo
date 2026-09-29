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
        (keepalive-seen nil)
        (spill-stream nil)
        (spill-path nil))
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
                       (setf spill-stream stream
                             spill-path (and (streamp stream)
                                             (probe-file (pathname stream)))
                             saw-spill (not (null spill-path)))
                       '(200 nil ("ok")))))
             (sb-ext:with-timeout 5
               (let ((client (usocket:socket-connect
                            "127.0.0.1" clack.test:*clack-test-port*
                            :element-type '(unsigned-byte 8))))
                 (unwind-protect
                      (let ((stream (usocket:socket-stream client)))
                      ;; Keep the connection open while the first response is
                      ;; consumed.  This makes the assertion below sensitive
                      ;; to response completion cleanup rather than the socket
                      ;; close hook.
                      (write-sequence
                       (trivial-utf-8:string-to-utf-8-bytes
                        (concatenate 'string
                                     (woo-test::crlf-lines
                                      "POST /spill HTTP/1.1" "Host: localhost"
                                      "Content-Length: 4" "Connection: keep-alive" "")
                                     "data"))
                       stream)
                      (force-output stream)
                      (loop for line =
                              (with-output-to-string (line)
                                (loop for byte = (read-byte stream nil nil)
                                      while byte
                                      until (= byte 10)
                                      unless (= byte 13)
                                        do (write-char (code-char byte) line)))
                            until (string= line ""))
                      (dotimes (i 2) (read-byte stream nil nil))
                      (ok (and spill-path
                               (search (string (car (last (pathname-directory dir))))
                                       (namestring spill-path))))
                      (loop repeat 100
                            until (and (not (open-stream-p spill-stream))
                                       (not (probe-file spill-path)))
                            do (sleep 0.01))
                      (ok (not (open-stream-p spill-stream)))
                      (ok (not (probe-file spill-path)))
                      (write-sequence
                       (trivial-utf-8:string-to-utf-8-bytes
                        (woo-test::crlf-lines "GET /next HTTP/1.1"
                                              "Host: localhost"
                                              "Connection: close" ""))
                       stream)
                      (force-output stream)
                        (loop while (read-byte stream nil nil)))
                   (ignore-errors (usocket:socket-close client))))))
           (ok saw-spill)
           (ok keepalive-seen)
           (ok (null (uiop:directory-files dir)))
           (let ((response (make-string (* 1024 1024) :initial-element #\x))
                 (abort-seen nil)
                 (abort-body-path nil)
                 (abort-body-stream nil))
             (clack.test:testing-app "Remove spilled request body after abort"
                 (lambda (env)
                   (if (string= (getf env :path-info) "/health")
                       '(200 nil ("healthy"))
                       (progn
                         (let ((stream (getf env :raw-body)))
                           (setf abort-body-stream stream
                                 abort-body-path (and (streamp stream)
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
                     until (and (not (open-stream-p abort-body-stream))
                                (not (probe-file abort-body-path)))
                     do (sleep 0.01))
               (ok (not (open-stream-p abort-body-stream)))
               (ok (not (probe-file abort-body-path)))
               (ok (null (uiop:directory-files dir)))
               (ok (search "HTTP/1.1 200 OK"
                           (woo-test::raw-exchange
                            clack.test:*clack-test-port*
                            (woo-test::crlf-lines "GET /health HTTP/1.1"
                                                  "Host: localhost" "Connection: close" "")))))
           (ok (null (uiop:directory-files dir))))
      (setf (symbol-value 'smart-buffer::*default-memory-limit*) old-memory
            (symbol-value 'smart-buffer::*default-disk-limit*) old-disk
            (symbol-value 'smart-buffer::*temporary-directory*) old-directory)
      (uiop:delete-directory-tree dir :validate t :if-does-not-exist :ignore)))))
