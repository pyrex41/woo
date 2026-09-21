(in-package :cl-user)
(defpackage woo-test
  (:use :cl
        :rove))
(in-package :woo-test)

(deftest woo-server-tests
  (clack.test.suite:run-server-tests :woo))

(deftest http2-preface-detection
  (testing "complete PRI preface is HTTP/2"
    (let ((preface woo.http2.constants:+connection-preface+))
      (ok (eq (woo:looks-like-http2-preface preface 0 (length preface)) :http2)
          "24-byte connection preface")
      (ok (woo:http2-connection-preface-match preface 0 24))))
  (testing "partial matching prefix needs more data"
    (let ((partial (subseq woo.http2.constants:+connection-preface+ 0 8)))
      (ok (eq (woo:looks-like-http2-preface partial 0 8) :need-more))))
  (testing "HTTP/1 request is not a preface"
    (let ((get (trivial-utf-8:string-to-utf-8-bytes "GET / HTTP/1.1")))
      (ok (eq (woo:looks-like-http2-preface get 0 (length get)) :http1))))
  (testing "empty buffer waits"
    (let ((empty (make-array 0 :element-type '(unsigned-byte 8))))
      (ok (eq (woo:looks-like-http2-preface empty 0 0) :need-more)))))

(deftest woo-ssl-server-tests
  (let ((clack.test:*clackup-additional-args*
          '(:ssl-cert-file #P"t/certs/localhost.crt"
            :ssl-key-file #P"t/certs/localhost.key"))
        (dex:*not-verify-ssl* t)
        (clack.test:*use-https* t))
    (clack.test.suite:run-server-tests :woo)))
