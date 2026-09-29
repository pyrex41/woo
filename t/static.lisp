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
