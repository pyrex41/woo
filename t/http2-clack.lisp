(in-package :cl-user)
(defpackage woo-test.http2-clack
  (:use :cl :rove)
  (:import-from :woo.http2.clack
                :build-clack-env
                :validate-request-headers
                :combine-header-fields
                :send-http2-response
                :connection-send-max-frame-size
                :*http2-frame-sink*)
  (:import-from :woo.http2.connection
                :make-http2-connection
                :http2-connection-streams
                :http2-connection-goaway-sent
                :connection-get-stream
                :connection-process-frame
                :http2-connection-remote-max-frame-size
                :http2-connection-local-max-frame-size
                :http2-connection-remote-window-size)
  (:import-from :woo.http2.stream
                :make-http2-stream
                :http2-stream-id
                :http2-stream-state
                :http2-stream-window-size
                :stream-closed-p
                :stream-half-closed-remote-p
                :+state-open+
                :+state-half-closed-local+
                :+state-half-closed-remote+)
  (:import-from :woo.http2.frames
                :frame-type
                :frame-flags
                :frame-payload
                :frame-length
                :make-headers-frame
                :make-data-frame
                :make-window-update-frame)
  (:import-from :woo.http2.hpack
                :make-hpack-context
                :hpack-decode-headers)
  (:import-from :woo.http2.constants
                :+frame-headers+
                :+frame-continuation+
                :+frame-data+
                :+flag-end-headers+
                :+flag-end-stream+
                :+protocol-error+
                :+stream-closed+))
(in-package :woo-test.http2-clack)

(defun valid-request-headers (&rest extra)
  (append (list (cons ":method" "GET")
                (cons ":scheme" "https")
                (cons ":path" "/foo")
                (cons ":authority" "example.com"))
          extra))

(deftest request-header-validation
  (testing "Required request pseudo-headers accepted"
    (ok (validate-request-headers (valid-request-headers))))
  (testing "Missing :path is rejected"
    (ng (validate-request-headers
         (list (cons ":method" "GET") (cons ":scheme" "https")))))
  (testing "Pseudo-header after regular header is rejected"
    (ng (validate-request-headers
         (list (cons ":method" "GET")
               (cons "host" "x")
               (cons ":scheme" "https")
               (cons ":path" "/")))))
  (testing ":status is not a request pseudo-header"
    (ng (validate-request-headers
         (list (cons ":method" "GET")
               (cons ":scheme" "https")
               (cons ":path" "/")
               (cons ":status" "200")))))
  (testing "Unknown pseudo-header rejected"
    (ng (validate-request-headers
         (list (cons ":method" "GET")
               (cons ":scheme" "https")
               (cons ":path" "/")
               (cons ":foo" "bar")))))
  (testing "connection header rejected"
    (ng (validate-request-headers
         (valid-request-headers (cons "connection" "keep-alive")))))
  (testing "transfer-encoding rejected"
    (ng (validate-request-headers
         (valid-request-headers (cons "transfer-encoding" "chunked")))))
  (testing "TE other than trailers rejected"
    (ng (validate-request-headers
         (valid-request-headers (cons "te" "gzip")))))
  (testing "TE: trailers allowed"
    (ok (validate-request-headers
         (valid-request-headers (cons "te" "trailers"))))))

(deftest duplicate-headers-combined
  (testing "Regular headers combine with comma"
    (let ((combined (combine-header-fields
                     (valid-request-headers
                      (cons "accept" "text/html")
                      (cons "accept" "application/json")))))
      (ok (equal (cdr (assoc "accept" combined :test #'string=))
                 "text/html, application/json"))))
  (testing "Cookie headers combine with semicolon"
    (let ((combined (combine-header-fields
                     (valid-request-headers
                      (cons "cookie" "a=1")
                      (cons "cookie" "b=2")))))
      (ok (equal (cdr (assoc "cookie" combined :test #'string=))
                 "a=1; b=2"))))
  (testing "build-clack-env stores combined accept"
    (let ((env (build-clack-env nil (make-http2-stream :id 1)
                                (valid-request-headers
                                 (cons "accept" "a")
                                 (cons "accept" "b")))))
      (ok env)
      (ok (equal (gethash "accept" (getf env :headers)) "a, b")))))

(deftest build-clack-env-rejects-invalid
  (testing "Invalid headers yield NIL env"
    (ok (null (build-clack-env nil (make-http2-stream :id 1)
                               (list (cons ":method" "GET")))))))

(deftest send-max-frame-size-is-min
  (testing "min of local and remote SETTINGS_MAX_FRAME_SIZE"
    (let ((conn (make-http2-connection)))
      (setf (http2-connection-local-max-frame-size conn) 16384
            (http2-connection-remote-max-frame-size conn) 100)
      (ok (= (connection-send-max-frame-size conn) 100))
      (setf (http2-connection-local-max-frame-size conn) 50
            (http2-connection-remote-max-frame-size conn) 16384)
      (ok (= (connection-send-max-frame-size conn) 50)))))

(deftest response-data-split-and-window
  (testing "DATA split at max frame size and send window decremented"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :window-size 1000))
           (frames nil)
           (body (make-array 250 :element-type '(unsigned-byte 8) :initial-element 7)))
      (setf (http2-connection-local-max-frame-size conn) 100
            (http2-connection-remote-max-frame-size conn) 100
            (http2-connection-remote-window-size conn) 1000)
      (let ((*http2-frame-sink* (lambda (f) (push f frames))))
        (send-http2-response conn stream 200 '(:content-type "text/plain") body))
      (setf frames (nreverse frames))
      (let ((data-frames (remove-if-not (lambda (f) (= (frame-type f) +frame-data+)) frames)))
        (ok (= (length data-frames) 3)
            "250 bytes at 100 max -> 3 DATA frames")
        (ok (every (lambda (f) (<= (frame-length f) 100)) data-frames)
            "No DATA payload exceeds max frame size")
        (ok (plusp (logand (frame-flags (car (last data-frames))) +flag-end-stream+))
            "Last DATA has END_STREAM")
        (ok (= (http2-connection-remote-window-size conn) (- 1000 250)))
        (ok (= (http2-stream-window-size stream) (- 1000 250)))))))

(deftest response-headers-split-continuation
  (testing "Large header block uses CONTINUATION"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :window-size 65535))
           (frames nil)
           (headers (list :x-long (make-string 200 :initial-element #\a))))
      (setf (http2-connection-local-max-frame-size conn) 32
            (http2-connection-remote-max-frame-size conn) 32)
      (let ((*http2-frame-sink* (lambda (f) (push f frames))))
        (send-http2-response conn stream 200 headers nil))
      (setf frames (nreverse frames))
      (ok (= (frame-type (first frames)) +frame-headers+))
      (ok (zerop (logand (frame-flags (first frames)) +flag-end-headers+))
          "First HEADERS lacks END_HEADERS when split")
      (ok (find-if (lambda (f) (= (frame-type f) +frame-continuation+)) frames)
          "CONTINUATION frames emitted")
      (let ((last (car (last frames))))
        (ok (plusp (logand (frame-flags last) +flag-end-headers+)))))))

(deftest send-window-zero-stops-data
  (testing "Zero send window does not emit DATA"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :window-size 0))
           (frames nil)
           (body (make-array 10 :element-type '(unsigned-byte 8) :initial-element 1)))
      (setf (http2-connection-remote-window-size conn) 0)
      (let ((*http2-frame-sink* (lambda (f) (push f frames))))
        (send-http2-response conn stream 200 nil body))
      (ok (not (find-if (lambda (f) (= (frame-type f) +frame-data+)) frames))
          "No DATA when window is 0"))))

(defun concat-octets (parts)
  (let* ((len (reduce #'+ parts :key #'length :initial-value 0))
         (out (make-array len :element-type '(unsigned-byte 8)))
         (i 0))
    (dolist (part parts out)
      (replace out part :start1 i)
      (incf i (length part)))))

(defun frames-of-type (frames type)
  (remove-if-not (lambda (f) (= (frame-type f) type)) frames))

(defun header-block-bytes (frames)
  (concat-octets
   (mapcar #'frame-payload
           (remove-if-not (lambda (f)
                            (or (= (frame-type f) +frame-headers+)
                                (= (frame-type f) +frame-continuation+)))
                          frames))))

(defun data-bytes (frames)
  (concat-octets (mapcar #'frame-payload (frames-of-type frames +frame-data+))))

(defun end-stream-p (frame)
  (plusp (logand (frame-flags frame) +flag-end-stream+)))

(defun register-stream (conn stream)
  (setf (gethash (http2-stream-id stream) (http2-connection-streams conn)) stream))

(defun capture-response (conn stream status headers body)
  (let ((frames nil))
    (let ((*http2-frame-sink* (lambda (f) (push f frames))))
      (values (send-http2-response conn stream status headers body)
              (nreverse frames)))))

(deftest malformed-request-headers
  (testing "NUL, CR, and LF in a field value are rejected"
    (dolist (bad (list (concatenate 'string "a" (string #\Nul) "b")
                       (format nil "a~%b")
                       (format nil "a~Cb" #\Return)))
      (ng (validate-request-headers
           (valid-request-headers (cons "x-a" bad)))
          bad)))
  (testing "Non-token field names are rejected"
    (ng (validate-request-headers
         (valid-request-headers (cons "bad name" "x"))))
    (ng (validate-request-headers
         (valid-request-headers (cons "bad/name" "x"))))
    (ok (validate-request-headers
         (valid-request-headers (cons "x-request-id" "abc")))))
  (testing ":path must be * or begin with /"
    (ok (validate-request-headers
         (list (cons ":method" "OPTIONS")
               (cons ":scheme" "https")
               (cons ":path" "*"))))
    (ng (validate-request-headers
         (list (cons ":method" "GET")
               (cons ":scheme" "https")
               (cons ":path" "foo")
               (cons ":authority" "example.com"))))
    (ng (validate-request-headers
         (list (cons ":method" "GET")
               (cons ":scheme" "https")
               (cons ":path" "*x")
               (cons ":authority" "example.com")))))
  (testing "Disagreeing :authority and host are rejected; case-insensitive match is not"
    (ng (validate-request-headers
         (valid-request-headers (cons "host" "other.test"))))
    (ng (validate-request-headers
         (valid-request-headers (cons "host" "example.com:443"))))
    (ok (validate-request-headers
         (valid-request-headers (cons "host" "example.com"))))
    (ok (validate-request-headers
         (valid-request-headers (cons "host" "Example.COM")))))
  (testing "Malformed headers RST before the app"
    (let ((called nil)
          (conn (make-http2-connection))
          (stream (make-http2-stream :id 1 :state +state-half-closed-remote+)))
      (register-stream conn stream)
      (woo.http2.clack::handle-http2-headers
       conn nil stream
       (valid-request-headers (cons "x-a" (format nil "a~%b")))
       t
       (lambda (env)
         (declare (ignore env))
         (setf called t)
         '(200 () "no")))
      (ok (not called))
      (ok (not (http2-connection-goaway-sent conn)))
      (ok (equal (woo.http2.connection::http2-connection-last-rst conn)
                 (cons 1 +protocol-error+)))
      (ok (stream-closed-p stream))
      (ok (null (gethash 1 (http2-connection-streams conn)))))))

(deftest authority-ipv6-split
  (testing "Bare bracketed IPv6 is not host [: port 1"
    (let ((env (build-clack-env
                nil (make-http2-stream :id 1)
                (list (cons ":method" "GET")
                      (cons ":scheme" "https")
                      (cons ":path" "/")
                      (cons ":authority" "[::1]")))))
      (ok (equal (getf env :server-name) "[::1]"))
      (ok (= (getf env :server-port) 443))
      (ok (/= (getf env :server-port) 1))))
  (testing "Port is taken only after the closing bracket"
    (let ((env (build-clack-env
                nil (make-http2-stream :id 1)
                (list (cons ":method" "GET")
                      (cons ":scheme" "https")
                      (cons ":path" "/")
                      (cons ":authority" "[::1]:8443")))))
      (ok (equal (getf env :server-name) "[::1]"))
      (ok (= (getf env :server-port) 8443))))
  (testing "Hostname :port and a bracketed Host header still split"
    (let ((named (build-clack-env
                  nil (make-http2-stream :id 1)
                  (list (cons ":method" "GET")
                        (cons ":scheme" "http")
                        (cons ":path" "/")
                        (cons ":authority" "example.com:8080"))))
          (host-only (build-clack-env
                      nil (make-http2-stream :id 1)
                      (list (cons ":method" "GET")
                            (cons ":scheme" "http")
                            (cons ":path" "/")
                            (cons "host" "[::1]")))))
      (ok (equal (getf named :server-name) "example.com"))
      (ok (= (getf named :server-port) 8080))
      (ok (equal (getf host-only :server-name) "[::1]"))
      (ok (= (getf host-only :server-port) 80))
      (ok (/= (getf host-only :server-port) 1)))))

(deftest response-closes-stream-and-frees-concurrency
  (testing "A full response on a finished request closes and drops the stream"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state +state-half-closed-remote+
                                      :window-size 1000)))
      (setf (http2-connection-remote-window-size conn) 1000)
      (register-stream conn stream)
      (multiple-value-bind (sent frames)
          (capture-response conn stream 200 '(:content-type "text/plain") "ok")
        (declare (ignore frames))
        (ok sent "fully sent body reports success"))
      (ok (stream-closed-p stream))
      (ok (null (gethash 1 (http2-connection-streams conn))))
      (ok (stream-closed-p (connection-get-stream conn 1)))
      (ok (not (http2-connection-goaway-sent conn)))))
  (testing "An open request becomes half-closed local and still counts"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state +state-open+ :window-size 100)))
      (setf (http2-connection-remote-window-size conn) 100)
      (register-stream conn stream)
      (ok (send-http2-response conn stream 200 nil "x"))
      (ok (= (http2-stream-state stream) +state-half-closed-local+))
      (ok (eq stream (gethash 1 (http2-connection-streams conn))))))
  (testing "Over MAX_CONCURRENT_STREAMS is RST, not GOAWAY"
    (let ((conn (make-http2-connection))
          (err nil))
      (setf (woo.http2.connection::http2-connection-local-max-concurrent-streams conn) 1)
      (setf (woo.http2.connection::http2-connection-on-error conn)
            (lambda (code debug)
              (declare (ignore debug))
              (setf err code)))
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t))
      (ok (null err))
      (let ((window (http2-stream-window-size (connection-get-stream conn 1))))
        (connection-process-frame
         conn (make-headers-frame 3 #() :end-headers t))
        (ok (= err +protocol-error+))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (equal (woo.http2.connection::http2-connection-last-rst conn)
                   (cons 3 +protocol-error+)))
        (ok (stream-closed-p (connection-get-stream conn 3)))
        (ok (null (gethash 3 (http2-connection-streams conn))))
        (connection-process-frame conn (make-window-update-frame 1 10))
        (ok (not (http2-connection-goaway-sent conn))
            "connection still accepts frames after the stream RST")
        (ok (= (http2-stream-window-size (connection-get-stream conn 1))
               (+ window 10))))))
  (testing "Closing a response lets the next stream open"
    (let ((conn (make-http2-connection)))
      (setf (woo.http2.connection::http2-connection-local-max-concurrent-streams conn) 1)
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t :end-stream t))
      (let ((stream (connection-get-stream conn 1)))
        (ok (stream-half-closed-remote-p stream))
        (ok (send-http2-response conn stream 200 nil "ok")))
      (ok (null (gethash 1 (http2-connection-streams conn))))
      (connection-process-frame
       conn (make-headers-frame 3 #() :end-headers t :end-stream t))
      (ok (not (http2-connection-goaway-sent conn)))
      (ok (stream-half-closed-remote-p (connection-get-stream conn 3)))))
  (testing "DATA after a completed response is RST STREAM_CLOSED, not GOAWAY"
    (let ((conn (make-http2-connection))
          (err nil))
      (setf (woo.http2.connection::http2-connection-on-error conn)
            (lambda (code debug)
              (declare (ignore debug))
              (setf err code)))
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t :end-stream t))
      (ok (send-http2-response conn (connection-get-stream conn 1) 200 nil nil))
      (connection-process-frame
       conn (make-data-frame 1 (make-array 1 :element-type '(unsigned-byte 8)
                                           :initial-element 1)))
      (ok (= err +stream-closed+))
      (ok (not (http2-connection-goaway-sent conn)))
      (ok (equal (woo.http2.connection::http2-connection-last-rst conn)
                 (cons 1 +stream-closed+))))))

(deftest queued-body-flushes-on-window-update
  (testing "Window 0 queues the body and does not report success"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state +state-half-closed-remote+
                                      :window-size 0))
           (body (make-array 10 :element-type '(unsigned-byte 8) :initial-element 9))
           (sent-parts nil))
      (setf (http2-connection-remote-window-size conn) 100)
      (register-stream conn stream)
      (multiple-value-bind (sent frames)
          (capture-response conn stream 200 nil body)
        (ok (not sent))
        (ok (not (frames-of-type frames +frame-data+)))
        (ok (stream-half-closed-remote-p stream))
        (ok (not (end-stream-p (find-if (lambda (f) (= (frame-type f) +frame-headers+))
                                        frames)))))
      (flet ((flush-update (increment)
               (let ((frames nil))
                 (let ((*http2-frame-sink* (lambda (f) (push f frames))))
                   (connection-process-frame
                    conn (make-window-update-frame 1 increment)))
                 (nreverse frames))))
        (let* ((first (flush-update 4))
               (data (frames-of-type first +frame-data+)))
          (ok (= (length data) 1))
          (ok (= (frame-length (first data)) 4))
          (ok (not (end-stream-p (first data))))
          (push (frame-payload (first data)) sent-parts))
        (ok (stream-half-closed-remote-p stream))
        (ok (gethash 1 (http2-connection-streams conn)))
        (let* ((second (flush-update 6))
               (data (frames-of-type second +frame-data+)))
          (ok (= (length data) 1))
          (ok (= (frame-length (first data)) 6))
          (ok (end-stream-p (first data)))
          (push (frame-payload (first data)) sent-parts)))
      (ok (equalp (concat-octets (nreverse sent-parts)) body))
      (ok (stream-closed-p stream))
      (ok (null (gethash 1 (http2-connection-streams conn))))))
  (testing "Connection WINDOW_UPDATE flushes a body blocked on the connection window"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state +state-half-closed-remote+
                                      :window-size 50))
           (body (make-array 5 :element-type '(unsigned-byte 8) :initial-element 3))
           (frames nil))
      (setf (http2-connection-remote-window-size conn) 0)
      (register-stream conn stream)
      (ok (not (send-http2-response conn stream 200 nil body)))
      (let ((*http2-frame-sink* (lambda (f) (push f frames))))
        (connection-process-frame conn (make-window-update-frame 0 5)))
      (setf frames (nreverse frames))
      (ok (equalp (data-bytes frames) body))
      (ok (end-stream-p (car (last (frames-of-type frames +frame-data+)))))
      (ok (stream-closed-p stream)))))

(deftest pathname-and-function-responses
  (testing "A pathname body is sent with content-type, not turned into 500"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state +state-half-closed-remote+
                                      :window-size 1000))
           (payload (make-array 4 :element-type '(unsigned-byte 8)
                                :initial-contents '(10 20 30 40)))
           (path (merge-pathnames "woo-h2-static.txt"
                                  (uiop:temporary-directory))))
      (setf (http2-connection-remote-window-size conn) 1000)
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output :if-exists :supersede
                                       :element-type '(unsigned-byte 8))
               (write-sequence payload out))
             (multiple-value-bind (sent frames)
                 (capture-response conn stream 200 nil path)
               (ok sent)
               (ok (equalp (data-bytes frames) payload))
               (let ((headers (hpack-decode-headers
                               (make-hpack-context)
                               (header-block-bytes frames))))
                 (ok (equal (cdr (assoc "content-type" headers :test #'string=))
                            (mimes:mime path)))
                 (ok (equal (cdr (assoc "content-length" headers :test #'string=))
                            "4")))))
        (when (probe-file path)
          (delete-file path)))))
  (testing "A Clack function response is invoked, not dropped"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state +state-half-closed-remote+
                                      :window-size 100))
           (frames nil)
           (saw-env nil))
      (setf (http2-connection-remote-window-size conn) 100)
      (register-stream conn stream)
      (let ((*http2-frame-sink* (lambda (f) (push f frames))))
        (woo.http2.clack::handle-http2-headers
         conn nil stream (valid-request-headers) t
         (lambda (env)
           (setf saw-env env)
           (lambda (responder)
             (funcall responder '(200 (:content-type "text/plain") "fn-body"))))))
      (setf frames (nreverse frames))
      (ok saw-env)
      (ok (equalp (data-bytes frames)
                  (map '(vector (unsigned-byte 8)) #'char-code "fn-body")))
      (ok (stream-closed-p stream))
      (ok (not (http2-connection-goaway-sent conn)))))
  (testing "A delayed response may still carry a pathname body"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 3 :state +state-half-closed-remote+
                                      :window-size 100))
           (payload (make-array 2 :element-type '(unsigned-byte 8)
                                :initial-contents '(7 8)))
           (path (merge-pathnames "woo-h2-delayed.txt"
                                  (uiop:temporary-directory)))
           (frames nil))
      (setf (http2-connection-remote-window-size conn) 100)
      (register-stream conn stream)
      (unwind-protect
           (progn
             (with-open-file (out path :direction :output :if-exists :supersede
                                       :element-type '(unsigned-byte 8))
               (write-sequence payload out))
             (let ((*http2-frame-sink* (lambda (f) (push f frames))))
               (woo.http2.clack::handle-http2-headers
                conn nil stream (valid-request-headers) t
                (lambda (env)
                  (declare (ignore env))
                  (lambda (responder)
                    (funcall responder (list 200 nil path))))))
             (ok (equalp (data-bytes (nreverse frames)) payload))
             (ok (stream-closed-p stream)))
        (when (probe-file path)
          (delete-file path))))))
