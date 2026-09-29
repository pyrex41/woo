(in-package :cl-user)
(defpackage woo-test.static
  (:use :cl :rove))
(in-package :woo-test.static)

(deftest pathname-response-has-a-body
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (file (merge-pathnames
               (format nil "woo-static-~36R.txt" (random (expt 36 8)))
               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (out file :direction :output :if-exists :error)
             (write-string "Woo private static fixture" out))
           (clack.test:testing-app "Serve a pathname response"
               (lambda (env)
                 (declare (ignore env))
                 `(200 (:content-type "text/plain") ,file))
             (let ((response
                     (woo-test::raw-exchange
                      clack.test:*clack-test-port*
                      (woo-test::crlf-lines
                       "GET /private-static HTTP/1.1"
                       "Host: localhost"
                       "Connection: close"
                       ""))))
               (ok (and (search "HTTP/1.1 200 OK" response)
                        (search "Woo private static fixture" response)))))
      (when (probe-file file) (delete-file file))))))

(deftest pathname-failures-have-distinct-statuses
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (root (asdf:system-source-directory :woo))
        (mismatch (merge-pathnames
                   (format nil "woo-static-mismatch-~36R.txt" (random (expt 36 8)))
                   (uiop:temporary-directory))))
    (with-open-file (out mismatch :direction :output :if-exists :error)
      (write-string "mismatch" out))
    (clack.test:testing-app "Classify missing files and directories"
        (lambda (env)
          (cond ((string= (getf env :path-info) "/missing")
                 `(200 nil ,(merge-pathnames #p"does-not-exist" root)))
                ((string= (getf env :path-info) "/mismatch")
                 `(200 (:content-length "999") ,mismatch))
                (t `(200 nil ,root))))
      (unwind-protect
           (let ((missing
              (woo-test::raw-exchange
               clack.test:*clack-test-port*
               (woo-test::crlf-lines "GET /missing HTTP/1.1" "Host: localhost"
                                     "Connection: close" "")))
            (mismatch-response
              (woo-test::raw-exchange
               clack.test:*clack-test-port*
               (woo-test::crlf-lines "GET /mismatch HTTP/1.1" "Host: localhost"
                                     "Connection: close" "")))
            (directory
              (woo-test::raw-exchange
               clack.test:*clack-test-port*
               (woo-test::crlf-lines "GET /directory HTTP/1.1" "Host: localhost"
                                     "Connection: close" ""))))
             (ok (and (search "HTTP/1.1 404 Not Found" missing)
                      (search "HTTP/1.1 500 Internal Server Error" mismatch-response)
                      (search "HTTP/1.1 403 Forbidden" directory))))
        (when (probe-file mismatch) (delete-file mismatch))))))

(deftest unreadable-path-is-forbidden
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (file (merge-pathnames
               (format nil "woo-static-eacces-~36R.txt" (random (expt 36 8)))
               (uiop:temporary-directory))))
    (unwind-protect
        (progn
          (with-open-file (out file :direction :output :if-exists :error)
            (write-string "private" out))
          (wsys:chmod (namestring file) 0)
          (clack.test:testing-app "Reject unreadable pathname"
              (lambda (env)
                (declare (ignore env))
                `(200 nil ,file))
            (let ((response
                    (woo-test::raw-exchange
                     clack.test:*clack-test-port*
                     (woo-test::crlf-lines
                      "GET /private HTTP/1.1" "Host: localhost"
                      "Connection: close" ""))))
              (ok (search "HTTP/1.1 403 Forbidden" response)))))
      (wsys:chmod (namestring file) #o644)
      (when (probe-file file) (delete-file file)))))
