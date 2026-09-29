(in-package :cl-user)
(defpackage woo-test.static
  (:use :cl :rove))
(in-package :woo-test.static)

(deftest pathname-response-has-a-body
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (file (asdf:system-relative-pathname :woo "README.md")))
    (clack.test:testing-app "Serve a pathname response"
        (lambda (env)
          (declare (ignore env))
          `(200 (:content-type "text/plain") ,file))
      (let ((response
              (woo-test::raw-exchange
               clack.test:*clack-test-port*
               (woo-test::crlf-lines
                "GET /README.md HTTP/1.1"
                "Host: localhost"
                "Connection: close"
                ""))))
        (ok (and (search "HTTP/1.1 200 OK" response)
                 (search "Woo" response)))))))

(deftest pathname-failures-have-distinct-statuses
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (root (asdf:system-source-directory :woo)))
    (clack.test:testing-app "Classify missing files and directories"
        (lambda (env)
          (if (string= (getf env :path-info) "/missing")
              `(200 nil ,(merge-pathnames #p"does-not-exist" root))
              `(200 nil ,root)))
      (let ((missing
              (woo-test::raw-exchange
               clack.test:*clack-test-port*
               (woo-test::crlf-lines "GET /missing HTTP/1.1" "Host: localhost"
                                     "Connection: close" "")))
            (directory
              (woo-test::raw-exchange
               clack.test:*clack-test-port*
               (woo-test::crlf-lines "GET /directory HTTP/1.1" "Host: localhost"
                                     "Connection: close" ""))))
        (ok (and (search "HTTP/1.1 404 Not Found" missing)
                 (search "HTTP/1.1 403 Forbidden" directory)))))))

(deftest unreadable-path-is-forbidden
  (let ((clack.test:*clack-test-handler* :woo)
        (clack.test:*enable-debug* nil)
        (file (merge-pathnames #p"README.md" (asdf:system-source-directory :woo))))
    (unwind-protect
        (progn
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
      (wsys:chmod (namestring file) #o644))))
