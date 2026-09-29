(in-package :cl-user)
(defpackage woo-test.tls-stream
  (:use :cl :rove))
(in-package :woo-test.tls-stream)

(deftest tls-static-stream-pumps-bounded-chunks
  (testing "TLS pathname sends keep at most one 64 KiB chunk pending"
    (let* ((path (merge-pathnames
                  (format nil "woo-tls-stream-~D.bin" (random 1000000))
                  #p"/tmp/"))
           (size (* 3 65536))
           (watchers (make-array 3 :initial-element (cffi:null-pointer)))
           (socket nil))
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :supersede)
               (dotimes (i size)
                 (write-byte (mod i 251) out)))
             (setf socket
                   (woo.ev.socket::%make-socket
                    :fd -1 :last-activity 0.0d0 :open-p t
                    :watchers watchers))
             (with-open-file (in path :element-type '(unsigned-byte 8))
               (woo.ev.socket:start-static-stream socket in size)
               (woo.ev.socket::pump-static-stream socket)
               (ok (= (woo.ev.socket::socket-send-stream-offset socket) 65536))
               (ok (<= (fast-io::output-buffer-len
                        (woo.ev.socket::socket-buffer socket))
                       65536))))
        (when (and socket (woo.ev.socket::socket-send-stream socket))
          (ignore-errors (close (woo.ev.socket::socket-send-stream socket)
                                :abort t)))
        (when (probe-file path)
          (delete-file path))))))

(deftest tls-pending-write-isolated-from-new-output
  (testing "application writes do not mutate bytes awaiting TLS retry"
    (let* ((socket (woo.ev.socket::%make-socket
                    :fd -1 :last-activity 0.0d0 :open-p t
                    :watchers (make-array 3 :initial-element (cffi:null-pointer))))
           (pending (make-array 4 :element-type '(unsigned-byte 8)
                                :initial-contents '(1 2 3 4))))
      (setf (woo.ev.socket::socket-pending-write-data socket) pending
            (woo.ev.socket::socket-pending-write-offset socket) 1)
      (woo.ev.socket:write-socket-data
       socket (make-array 2 :element-type '(unsigned-byte 8)
                          :initial-contents '(9 8)))
      (ok (equalp pending #(1 2 3 4)))
      (ok (= (woo.ev.socket::socket-pending-write-offset socket) 1))
      (ok (= (fast-io::output-buffer-len
              (woo.ev.socket::socket-buffer socket)) 2)))))

(deftest tls-ffi-retry-hooks-are-injectable
  (testing "WANT results can be supplied without changing the OpenSSL ABI"
    (let ((calls 0))
      (let ((woo.ev.socket::*ssl-write-function*
              (lambda (handle pointer length)
                (declare (ignore handle pointer length))
                (incf calls)
                -1))
            (woo.ev.socket::*ssl-error-function*
              (lambda (handle result)
                (declare (ignore handle result))
                cl+ssl::+ssl-error-want-read+)))
        (ok (= (funcall woo.ev.socket::*ssl-write-function* nil nil 8) -1))
        (ok (= (funcall woo.ev.socket::*ssl-error-function* nil -1)
               cl+ssl::+ssl-error-want-read+))
        (ok (= calls 1))))))

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

(deftest tls-alpn-setup-failure-releases-listener-context
  (testing "a failure after ALPN setup releases the listener context"
    (let ((create (symbol-function 'woo.ssl:create-context))
          (configure (symbol-function 'woo.ssl:configure-context-alpn))
          (free (symbol-function 'woo.ssl:free-context))
          (context nil)
          (configured nil)
          (freed 0)
          (baseline (hash-table-count woo.ssl.alpn::*alpn-ctx-args*)))
      (unwind-protect
           (progn
             (setf (symbol-function 'woo.ssl:create-context)
                   (lambda (&rest args)
                     (setf context (apply create args)))
                   (symbol-function 'woo.ssl:configure-context-alpn)
                   (lambda (&rest args)
                     (apply configure args)
                     (setf configured t)
                     (error "Injected ALPN setup failure"))
                   (symbol-function 'woo.ssl:free-context)
                   (lambda (ctx)
                     (funcall free ctx)
                     (incf freed)
                     (setf context nil)))
             (ok (signals
                   (woo:run (lambda (env)
                              (declare (ignore env))
                              '(200 nil ("ok")))
                            :debug nil
                            :worker-num nil
                            :ssl-cert-file #P"t/certs/localhost.crt"
                            :ssl-key-file #P"t/certs/localhost.key")
                   'error))
             (ok configured)
             (ok (= freed 1))
             (ok (= baseline (hash-table-count woo.ssl.alpn::*alpn-ctx-args*))))
        (setf (symbol-function 'woo.ssl:create-context) create
              (symbol-function 'woo.ssl:configure-context-alpn) configure
              (symbol-function 'woo.ssl:free-context) free)
        (when context (funcall free context))))))

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
