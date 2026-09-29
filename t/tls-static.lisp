(in-package :cl-user)
(defpackage woo-test.tls-stream
  (:use :cl :rove))
(in-package :woo-test.tls-stream)

(deftest tls-pathname-live-exact-body-and-clean-eof
  (testing "a live TLS pathname response preserves bytes and closes cleanly"
    (let* ((path (merge-pathnames
                  (format nil "woo-tls-live-~D.bin" (random 1000000))
                  (uiop:temporary-directory)))
           (size (* 256 1024)))
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :supersede)
               (dotimes (i size)
                 (write-byte (mod (+ i 17) 251) out)))
             (let ((clack.test:*clack-test-handler* :woo)
                   (clack.test:*use-https* t)
                   (clack.test:*clackup-additional-args*
                     (list :ssl-cert-file #P"t/certs/localhost.crt"
                           :ssl-key-file #P"t/certs/localhost.key")))
               (clack.test:testing-app "TLS pathname exact body"
                   (lambda (env)
                     (declare (ignore env))
                     `(200 (:content-type "application/octet-stream") ,path))
                 (usocket:with-client-socket (sock stream "127.0.0.1"
                                                     clack.test:*clack-test-port*
                                                     :element-type '(unsigned-byte 8))
                   (let ((tls (cl+ssl:make-ssl-client-stream
                               stream :hostname "localhost" :verify nil)))
                     (unwind-protect
                          (progn
                            (write-sequence
                             (trivial-utf-8:string-to-utf-8-bytes
                              (format nil "GET / HTTP/1.1~C~CHost: localhost~C~CConnection: close~C~C~C~C"
                                      #\Return #\Newline #\Return #\Newline
                                      #\Return #\Newline #\Return #\Newline))
                             tls)
                            (force-output tls)
                            (sb-ext:with-timeout 15
                              (labels ((read-header-line ()
                                         (let ((line (make-array 0 :element-type 'character
                                                                  :adjustable t :fill-pointer 0)))
                                           (loop for b = (read-byte tls nil nil)
                                                 while (and b (/= b 10))
                                                 do (unless (= b 13)
                                                      (vector-push-extend (code-char b) line)))
                                           (coerce line 'simple-string))))
                                (let* ((status (read-header-line))
                                       (headers nil))
                                  (loop for line = (read-header-line)
                                        until (zerop (length line))
                                        do (let ((split (position #\: line)))
                                             (when split
                                               (push (cons (string-downcase (subseq line 0 split))
                                                           (string-trim '(#\Space #\Tab)
                                                                        (subseq line (1+ split))))
                                                     headers))))
                                  (ok (search "HTTP/1.1 200" status))
                                  (ok (= (parse-integer (cdr (assoc "content-length" headers
                                                                     :test #'string=)))
                                         size))
                                  (let ((got (make-array size :element-type '(unsigned-byte 8)))
                                        (chunk (make-array 4096 :element-type '(unsigned-byte 8)))
                                        (offset 0))
                                    (loop while (< offset size)
                                          do (let* ((want (min 4096 (- size offset)))
                                                    (n (read-sequence chunk tls :start 0 :end want)))
                                               (ok (plusp n))
                                               (replace got chunk :start1 offset
                                                        :end1 (+ offset n) :end2 n)
                                               (incf offset n)
                                               ;; Keep the peer backpressured between
                                               ;; bounded reads.
                                               (sleep 0.001)))
                                    (ok (= offset size))
                                    (ok (loop for i below size
                                              always (= (aref got i) (mod (+ i 17) 251)))))
                                  ;; Require close_notify/EOF after the exact body.
                                  (ok (null (read-byte tls nil nil)))))))
                       (ignore-errors (close tls :abort t)))))))))
        (when (probe-file path)
          (delete-file path)))))

(deftest tls-static-preparation-failures-are-wire-visible
  (testing "TLS pathname preparation reports missing, directory, EACCES, and length errors"
    (let* ((missing (merge-pathnames
                     (format nil "woo-tls-missing-~36R" (random (expt 36 8)))
                     (uiop:temporary-directory)))
           (directory (uiop:temporary-directory))
           (private (merge-pathnames
                     (format nil "woo-tls-private-~36R.txt" (random (expt 36 8)))
                     (uiop:temporary-directory)))
           (mismatch (merge-pathnames
                      (format nil "woo-tls-mismatch-~36R.txt" (random (expt 36 8)))
                      (uiop:temporary-directory))))
      (with-open-file (out private :direction :output :if-exists :error)
        (write-string "private" out))
      (with-open-file (out mismatch :direction :output :if-exists :error)
        (write-string "mismatch" out))
      (unwind-protect
           (progn
             (wsys:chmod (namestring private) 0)
             (let ((clack.test:*clack-test-handler* :woo)
                   (clack.test:*use-https* t)
                   (clack.test:*clackup-additional-args*
                     (list :ssl-cert-file #P"t/certs/localhost.crt"
                           :ssl-key-file #P"t/certs/localhost.key")))
               (clack.test:testing-app "TLS pathname preparation failures"
                   (lambda (env)
                     (cond ((string= (getf env :path-info) "/missing")
                            `(200 nil ,missing))
                           ((string= (getf env :path-info) "/directory")
                            `(200 nil ,directory))
                           ((string= (getf env :path-info) "/private")
                            `(200 nil ,private))
                           (t `(200 (:content-length "999") ,mismatch))))
                 (labels ((request (path)
                            (usocket:with-client-socket
                                (sock stream "127.0.0.1" clack.test:*clack-test-port*
                                       :element-type '(unsigned-byte 8))
                              (let ((tls (cl+ssl:make-ssl-client-stream
                                          stream :hostname "localhost" :verify nil)))
                                (unwind-protect
                                     (progn
                                       (write-sequence
                                        (trivial-utf-8:string-to-utf-8-bytes
                                         (format nil "GET ~A HTTP/1.1~C~CHost: localhost~C~CConnection: close~C~C~C~C"
                                                 path #\Return #\Newline #\Return #\Newline
                                                 #\Return #\Newline #\Return #\Newline))
                                        tls)
                                       (force-output tls)
                                       (with-output-to-string (out)
                                         (loop for b = (read-byte tls nil nil)
                                               while b do (write-char (code-char b) out))))
                                  (ignore-errors (close tls :abort t)))))))
                   (ok (search "404 Not Found" (request "/missing")))
                   (ok (search "403 Forbidden" (request "/directory")))
                   (ok (search "403 Forbidden" (request "/private")))
                   (ok (search "500 Internal Server Error" (request "/mismatch"))))))
        (wsys:chmod (namestring private) #o644)
        (dolist (path (list private mismatch))
          (when (probe-file path) (delete-file path))))))))

