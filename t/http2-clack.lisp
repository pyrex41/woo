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
                :+refused-stream+
                :+stream-closed+))
(in-package :woo-test.http2-clack)

(defun empty-octets ()
  "Empty frame payload. A literal #() is a simple-vector, not octets."
  (make-array 0 :element-type '(unsigned-byte 8)))

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
  (testing "Leading or trailing SP/HTAB is a malformed field value"
    (dolist (bad (list " x" "x " (format nil "~Cx" #\Tab) (format nil "x~C" #\Tab)
                       " "))
      (ng (validate-request-headers
           (valid-request-headers (cons "x-a" bad)))
          bad))
    (ok (validate-request-headers
         (valid-request-headers (cons "x-a" "a b")))
        "interior space stays legal")
    (ok (validate-request-headers
         (valid-request-headers (cons "x-a" "")))
        "empty value is not leading whitespace"))
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
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (ok (null err))
      (let ((window (http2-stream-window-size (connection-get-stream conn 1))))
        (connection-process-frame
         conn (make-headers-frame 3 (empty-octets) :end-headers t))
        (ok (= err +refused-stream+))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (equal (woo.http2.connection::http2-connection-last-rst conn)
                   (cons 3 +refused-stream+)))
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
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (let ((stream (connection-get-stream conn 1)))
        (ok (stream-half-closed-remote-p stream))
        (ok (send-http2-response conn stream 200 nil "ok")))
      (ok (null (gethash 1 (http2-connection-streams conn))))
      (connection-process-frame
       conn (make-headers-frame 3 (empty-octets) :end-headers t :end-stream t))
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
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
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

;;; Trailers through the Clack adapter

(defun adapter-conn (app)
  "A connection wired to APP exactly as make-http2-app-handler wires it."
  (woo.http2.clack:attach-http2-app (make-http2-connection) nil app))

(defun request-block (headers)
  (woo.http2.hpack:hpack-encode-headers (make-hpack-context) headers))

(deftest request-trailers
  (testing "HEADERS, DATA, trailers with END_STREAM run the app once with the request"
    (let* ((envs nil)
           (conn (adapter-conn (lambda (env)
                                 (push env envs)
                                 '(200 (:content-type "text/plain") "ok")))))
      (let ((frames nil))
        (let ((*http2-frame-sink* (lambda (f) (push f frames))))
          (connection-process-frame
           conn (make-headers-frame 1 (request-block
                                       '((":method" . "POST")
                                         (":scheme" . "https")
                                         (":path" . "/upload?x=1")
                                         (":authority" . "example.com")))
                                    :end-headers t))
          (connection-process-frame
           conn (make-data-frame 1 (map '(vector (unsigned-byte 8)) #'char-code "hello")))
          (connection-process-frame
           conn (make-headers-frame 1 (request-block '(("x-checksum" . "abc")))
                                    :end-headers t :end-stream t)))
        (ok (= (length envs) 1) "the app runs once, on the trailers")
        (let ((env (first envs)))
          (ok (eq (getf env :request-method) :post))
          (ok (equal (getf env :path-info) "/upload"))
          (ok (equal (getf env :query-string) "x=1"))
          (ok (equal (map 'string #'code-char (getf env :raw-body)) "hello"))
          (ok (equal (woo.http2.stream:http2-stream-trailers (getf env :http2.stream))
                     '(("x-checksum" . "abc"))))
          (ok (equal (getf env :http2.trailers) '(("x-checksum" . "abc")))
              "the app sees the trailers in the env"))
        (ok (null (woo.http2.connection::http2-connection-last-rst conn))
            "no RST PROTOCOL_ERROR")
        (ok (not (http2-connection-goaway-sent conn)))
        (let ((response (hpack-decode-headers
                         (make-hpack-context)
                         (header-block-bytes (reverse frames)))))
          (ok (equal (cdr (assoc ":status" response :test #'string=)) "200"))))))

  (testing "trailers with a pseudo-header are malformed"
    (let* ((called nil)
           (conn (adapter-conn (lambda (env)
                                 (declare (ignore env))
                                 (setf called t)
                                 '(200 () "no")))))
      (connection-process-frame
       conn (make-headers-frame 1 (request-block (valid-request-headers))
                                :end-headers t))
      (connection-process-frame
       conn (make-headers-frame 1 (request-block '((":path" . "/other")))
                                :end-headers t :end-stream t))
      (ok (not called))
      (ok (equal (woo.http2.connection::http2-connection-last-rst conn)
                 (cons 1 +protocol-error+)))
      (ok (not (http2-connection-goaway-sent conn)))))

  (testing "a request without trailers has :http2.trailers NIL"
    (let* ((envs nil)
           (conn (adapter-conn (lambda (env)
                                 (push env envs)
                                 '(200 () "ok")))))
      (connection-process-frame
       conn (make-headers-frame 1 (request-block (valid-request-headers))
                                :end-headers t))
      (connection-process-frame
       conn (make-data-frame 1 (map '(vector (unsigned-byte 8)) #'char-code "x")
                             :end-stream t))
      (connection-process-frame
       conn (make-headers-frame 3 (request-block (valid-request-headers))
                                :end-headers t :end-stream t))
      (ok (= (length envs) 2))
      (dolist (env envs)
        (ok (member :http2.trailers env))
        (ok (null (getf env :http2.trailers))))))

  (testing "trailers with connection-specific or malformed fields do not reach the app"
    (dolist (field (list '("connection" . "close")
                         '("transfer-encoding" . "chunked")
                         '("te" . "gzip")
                         '("X-Upper" . "v")
                         (cons "x-a" (format nil "a~Cb" #\Return))
                         '("x-a" . " lead")))
      (let* ((called nil)
             (conn (adapter-conn (lambda (env)
                                   (declare (ignore env))
                                   (setf called t)
                                   '(200 () "no")))))
        (connection-process-frame
         conn (make-headers-frame 1 (request-block (valid-request-headers))
                                  :end-headers t))
        (connection-process-frame
         conn (make-headers-frame 1 (request-block (list field))
                                  :end-headers t :end-stream t))
        (ok (not called) (car field))
        (ok (equal (woo.http2.connection::http2-connection-last-rst conn)
                   (cons 1 +protocol-error+))
            (car field))
        (ok (not (http2-connection-goaway-sent conn))))))

  (testing "trailers without END_STREAM are malformed"
    (let* ((called nil)
           (conn (adapter-conn (lambda (env)
                                 (declare (ignore env))
                                 (setf called t)
                                 '(200 () "no")))))
      (connection-process-frame
       conn (make-headers-frame 1 (request-block (valid-request-headers))
                                :end-headers t))
      (connection-process-frame
       conn (make-headers-frame 1 (request-block '(("x-a" . "b")))
                                :end-headers t))
      (ok (not called))
      (ok (equal (woo.http2.connection::http2-connection-last-rst conn)
                 (cons 1 +protocol-error+))))))

;;; h2c prior knowledge against a running server, over a real socket.

(defparameter *h2c-deadline-seconds* 10
  "Upper bound for any one wait on the server in the socket tests.")

(defstruct h2c-client socket stream (buffer (make-array 0 :element-type '(unsigned-byte 8)
                                                          :adjustable t :fill-pointer 0))
  (closed nil))

(defun h2c-connect ()
  (let ((socket (usocket:socket-connect "127.0.0.1" clack.test:*clack-test-port*
                                        :element-type '(unsigned-byte 8))))
    (make-h2c-client :socket socket :stream (usocket:socket-stream socket))))

(defun h2c-send (client &rest frames)
  (let ((out (h2c-client-stream client)))
    (dolist (frame frames)
      (write-sequence (if (typep frame 'woo.http2.frames:frame)
                          (woo.http2.frames:serialize-frame frame)
                          frame)
                      out))
    (finish-output out)))

(defun h2c-read-frames (client)
  "Wait up to 0.2 s for input. Returns the complete frames now buffered.
   Sets CLOSED when the server has closed the connection."
  (let ((stream (h2c-client-stream client))
        (buf (h2c-client-buffer client)))
    (when (and (not (h2c-client-closed client))
               (usocket:wait-for-input (h2c-client-socket client)
                                       :timeout 0.2 :ready-only t))
      (handler-case
          (loop for byte = (read-byte stream nil :eof)
                do (if (eq byte :eof)
                       (progn (setf (h2c-client-closed client) t) (return))
                       (vector-push-extend byte buf))
                while (listen stream))
        (error () (setf (h2c-client-closed client) t))))
    (let ((frames nil))
      (loop
        (multiple-value-bind (frame consumed)
            (woo.http2.frames:parse-frame (coerce buf '(simple-array (unsigned-byte 8) (*))))
          (unless frame (return))
          (push frame frames)
          (replace buf buf :start2 consumed)
          (setf (fill-pointer buf) (- (length buf) consumed))))
      (nreverse frames))))

(defun h2c-read-until (client predicate)
  "Read frames until PREDICATE is true of the list so far, the server closes,
   or the deadline passes. Returns all frames read."
  (let ((deadline (+ (get-internal-real-time)
                     (* *h2c-deadline-seconds* internal-time-units-per-second)))
        (frames nil))
    (loop
      (setf frames (append frames (h2c-read-frames client)))
      (when (or (funcall predicate frames)
                (h2c-client-closed client)
                (> (get-internal-real-time) deadline))
        (return frames)))))

(defun h2c-preface-and-settings (client)
  (h2c-send client woo.http2.constants:+connection-preface+
            (woo.http2.frames:make-settings-frame nil)))

(defun window-update-increment (frame)
  (woo.http2.frames:parse-window-update-payload (frame-payload frame)))

(defun h2c-upload (client stream-id bytes)
  "Send BYTES as DATA, waiting for WINDOW_UPDATE credit like a real client.
   Returns (values sent-all-p frames-read)."
  (let ((conn-credit 65535)
        (stream-credit 65535)
        (offset 0)
        (seen nil)
        (deadline (+ (get-internal-real-time)
                     (* *h2c-deadline-seconds* internal-time-units-per-second))))
    (loop while (< offset (length bytes))
          do (let ((chunk (min 16384 (- (length bytes) offset) conn-credit stream-credit)))
               (cond
                 ((plusp chunk)
                  (h2c-send client (make-data-frame stream-id
                                                    (subseq bytes offset (+ offset chunk))))
                  (decf conn-credit chunk)
                  (decf stream-credit chunk)
                  (incf offset chunk))
                 ((or (h2c-client-closed client)
                      (> (get-internal-real-time) deadline))
                  (return-from h2c-upload (values nil seen)))
                 (t
                  (dolist (frame (h2c-read-frames client))
                    (push frame seen)
                    (when (= (frame-type frame) woo.http2.constants:+frame-window-update+)
                      (let ((sid (woo.http2.frames:frame-stream-id frame))
                            (inc (window-update-increment frame)))
                        (cond ((zerop sid) (incf conn-credit inc))
                              ((= sid stream-id) (incf stream-credit inc))))))))))
    (values t (nreverse seen))))

(defun frame-on-stream-p (frame stream-id type)
  (and (= (frame-type frame) type)
       (= (woo.http2.frames:frame-stream-id frame) stream-id)))

(deftest h2c-socket-end-to-end
  (let ((clack.test:*clack-test-handler* :woo))
    (clack.test:testing-app "an upload over 64 KB with trailers gets a response"
        (lambda (env)
          (let ((body (getf env :raw-body)))
            `(200 (:content-type "text/plain")
                  (,(format nil "~A ~A ~D"
                            (getf env :request-method)
                            (getf env :path-info)
                            (if body (length body) 0))))))
      (let ((client (h2c-connect))
            (bytes (make-array 200000 :element-type '(unsigned-byte 8)
                                      :initial-element 120)))
        (unwind-protect
             (progn
               (h2c-preface-and-settings client)
               (h2c-send client
                         (make-headers-frame 1 (request-block
                                                '((":method" . "POST")
                                                  (":scheme" . "http")
                                                  (":path" . "/upload")
                                                  (":authority" . "localhost")))
                                             :end-headers t))
               (multiple-value-bind (sent-all seen) (h2c-upload client 1 bytes)
                 (ok sent-all "the server kept granting credit for 200,000 octets")
                 (ok (find-if (lambda (f) (frame-on-stream-p
                                           f 0 woo.http2.constants:+frame-window-update+))
                              seen)
                     "connection WINDOW_UPDATE on the wire")
                 (h2c-send client (make-headers-frame 1 (request-block '(("x-sum" . "1")))
                                                      :end-headers t :end-stream t))
                 (let* ((frames (append seen
                                        (h2c-read-until
                                         client
                                         (lambda (fs)
                                           (find-if (lambda (f)
                                                      (and (frame-on-stream-p f 1 +frame-data+)
                                                           (end-stream-p f)))
                                                    fs)))))
                        (response (frames-of-type
                                   (remove-if-not (lambda (f)
                                                    (= (woo.http2.frames:frame-stream-id f) 1))
                                                  frames)
                                   +frame-headers+)))
                   (ok (not (find woo.http2.constants:+frame-goaway+ frames
                                  :key #'frame-type)))
                   (ok (not (find woo.http2.constants:+frame-rst-stream+ frames
                                  :key #'frame-type)))
                   (ok response)
                   (when response
                     (ok (equal (cdr (assoc ":status"
                                            (hpack-decode-headers (make-hpack-context)
                                                                  (frame-payload (first response)))
                                            :test #'string=))
                                "200")))
                   (ok (equal (map 'string #'code-char
                                   (data-bytes (remove-if-not
                                                (lambda (f)
                                                  (= (woo.http2.frames:frame-stream-id f) 1))
                                                frames)))
                              "POST /upload 200000")))))
          (usocket:socket-close (h2c-client-socket client))))))

  (let ((clack.test:*clack-test-handler* :woo))
    (clack.test:testing-app "a connection error writes GOAWAY, then closes the socket"
        (lambda (env) (declare (ignore env)) '(200 () ("ok")))
      (let ((client (h2c-connect)))
        (unwind-protect
             (progn
               (h2c-preface-and-settings client)
               (h2c-send client (make-window-update-frame 0 0))
               (let* ((frames (h2c-read-until client (constantly nil)))
                      (goaway (find woo.http2.constants:+frame-goaway+ frames
                                    :key #'frame-type)))
                 (ok goaway "GOAWAY is flushed before the close")
                 (when goaway
                   (ok (= (nth-value 1 (woo.http2.frames:parse-goaway-payload
                                        (frame-payload goaway)))
                          +protocol-error+)))
                 (ok (h2c-client-closed client) "the server closed the TCP connection")))
          (usocket:socket-close (h2c-client-socket client)))))))

;;; Streaming and delayed responses (the Clack responder protocol)

(defun octets (string)
  (map '(vector (unsigned-byte 8)) #'char-code string))

(defun frames-on-stream (frames stream-id)
  (remove-if-not (lambda (f) (= (woo.http2.frames:frame-stream-id f) stream-id))
                 frames))

(defun run-adapter-request (conn stream-id &key (headers (valid-request-headers)) body)
  "Send a request on CONN and return the frames written, oldest first."
  (let ((frames nil))
    (let ((*http2-frame-sink* (lambda (f) (push f frames))))
      (connection-process-frame
       conn (make-headers-frame stream-id (request-block headers)
                                :end-headers t :end-stream (null body)))
      (when body
        (connection-process-frame
         conn (make-data-frame stream-id (octets body) :end-stream t))))
    (nreverse frames)))

(defun capture-frames (thunk)
  (let ((frames nil))
    (let ((*http2-frame-sink* (lambda (f) (push f frames))))
      (funcall thunk))
    (nreverse frames)))

(defun response-status (frames)
  (cdr (assoc ":status" (hpack-decode-headers (make-hpack-context)
                                              (header-block-bytes frames))
              :test #'string=)))

(deftest streaming-responder
  (testing "(status headers) returns a writer; chunks go out as DATA, :close ends the stream"
    (let* ((writer nil)
           (conn (adapter-conn
                  (lambda (env)
                    (declare (ignore env))
                    (lambda (responder)
                      (setf writer (funcall responder '(200 (:content-type "text/plain"))))
                      (funcall writer "one ")
                      (funcall writer (octets "two "))
                      (funcall writer "three" :close t)))))
           (frames (run-adapter-request conn 1))
           (headers (frames-of-type frames +frame-headers+))
           (data (frames-of-type frames +frame-data+)))
      (ok (functionp writer) "the responder returns a writer function")
      (ok (= (length headers) 1) "exactly one HEADERS: no second 500 response")
      (ok (equal (response-status frames) "200"))
      (ok (not (end-stream-p (first headers))) "HEADERS does not end the stream")
      (ok (equal (map 'string #'code-char (data-bytes frames)) "one two three"))
      (ok (= (count-if #'end-stream-p data) 1) "one END_STREAM")
      (ok (end-stream-p (car (last data))) "on the last DATA frame")
      (ok (not (find woo.http2.constants:+frame-rst-stream+ frames :key #'frame-type)))
      (ok (null (gethash 1 (http2-connection-streams conn))) "the stream is closed and dropped")))

  (testing "a writer kept by the app writes later, and nil :close sends an empty END_STREAM"
    (let* ((writer nil)
           (conn (adapter-conn
                  (lambda (env)
                    (declare (ignore env))
                    (lambda (responder)
                      (setf writer (funcall responder '(200 ())))))))
           (first-frames (run-adapter-request conn 1)))
      (ok (= (length (frames-of-type first-frames +frame-headers+)) 1))
      (ok (null (frames-of-type first-frames +frame-data+)))
      (ok (gethash 1 (http2-connection-streams conn)) "the stream stays open for the writer")
      (let ((later (capture-frames (lambda ()
                                     (funcall writer "abc")
                                     (funcall writer "defgh" :start 1 :end 3)
                                     (funcall writer nil :close t)))))
        (ok (equal (map 'string #'code-char (data-bytes later)) "abcef"))
        (ok (end-stream-p (car (last later))))
        (ok (zerop (frame-length (car (last later)))) "END_STREAM on an empty DATA frame"))
      (ok (null (gethash 1 (http2-connection-streams conn))))
      (ok (null (capture-frames (lambda () (funcall writer "late"))))
          "writes after :close send nothing")))

  (testing "a streaming write past the send window is queued and flushed on WINDOW_UPDATE"
    (let* ((writer nil)
           (conn (adapter-conn
                  (lambda (env)
                    (declare (ignore env))
                    (lambda (responder)
                      (setf writer (funcall responder '(200 ()))))))))
      (run-adapter-request conn 1)
      (let ((stream (gethash 1 (http2-connection-streams conn))))
        (setf (http2-stream-window-size stream) 5)
        (let ((frames (capture-frames (lambda () (funcall writer "0123456789" :close t)))))
          (ok (equal (map 'string #'code-char (data-bytes frames)) "01234"))
          (ok (notany #'end-stream-p frames)))
        (let ((frames (capture-frames
                       (lambda ()
                         (connection-process-frame conn (make-window-update-frame 1 100))))))
          (ok (equal (map 'string #'code-char (data-bytes frames)) "56789"))
          (ok (end-stream-p (car (last frames))))))))

  (testing "a second response on the same stream is refused"
    (let* ((conn (adapter-conn
                  (lambda (env)
                    (declare (ignore env))
                    (lambda (responder)
                      (let ((writer (funcall responder '(200 ()))))
                        (funcall responder '(201 () "again"))
                        (funcall writer "only" :close t))))))
           (frames (run-adapter-request conn 1)))
      (ok (= (length (frames-of-type frames +frame-headers+)) 1))
      (ok (equal (response-status frames) "200"))
      (ok (equal (map 'string #'code-char (data-bytes frames)) "only"))))

  (testing "an app error after streaming began resets the stream instead of a second response"
    (let* ((conn (adapter-conn
                  (lambda (env)
                    (declare (ignore env))
                    (lambda (responder)
                      (funcall (funcall responder '(200 ())) "partial")
                      (error "boom")))))
           (frames (run-adapter-request conn 1))
           (rst (find woo.http2.constants:+frame-rst-stream+ frames :key #'frame-type)))
      (ok (= (length (frames-of-type frames +frame-headers+)) 1) "no second HEADERS")
      (ok rst "RST_STREAM ends the unfinished response")
      (when rst
        (ok (= (woo.http2.frames:parse-rst-stream-payload (frame-payload rst))
               woo.http2.constants:+internal-error+)))))

  (testing "writes after the peer resets the stream send nothing"
    (let* ((writer nil)
           (conn (adapter-conn
                  (lambda (env)
                    (declare (ignore env))
                    (lambda (responder)
                      (setf writer (funcall responder '(200 ()))))))))
      (run-adapter-request conn 1)
      (connection-process-frame conn (woo.http2.frames:make-rst-stream-frame
                                      1 woo.http2.constants:+cancel+))
      (ok (null (capture-frames (lambda ()
                                  (funcall writer "x")
                                  (funcall writer "y" :close t))))))))

(deftest no-frames-on-closed-streams
  (testing "send-http2-response on a closed stream sends nothing"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state woo.http2.stream:+state-closed+
                                      :window-size 100)))
      (multiple-value-bind (sent frames) (capture-response conn stream 200 nil "x")
        (ok (not sent))
        (ok (null frames)))))
  (testing "a response after the stream ended is not sent"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state +state-open+ :window-size 100)))
      (register-stream conn stream)
      (ok (send-http2-response conn stream 200 nil "first"))
      (multiple-value-bind (sent frames) (capture-response conn stream 500 nil "second")
        (ok (not sent))
        (ok (null frames)))))
  (testing "a queued body is dropped, not sent, once the peer resets the stream"
    (let* ((conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state +state-half-closed-remote+
                                      :window-size 0)))
      (register-stream conn stream)
      (setf (woo.http2.connection::http2-connection-last-stream-id conn) 1)
      (ok (not (send-http2-response conn stream 200 nil "queued")))
      (connection-process-frame conn (woo.http2.frames:make-rst-stream-frame
                                      1 woo.http2.constants:+cancel+))
      (ok (null (frames-of-type
                 (capture-frames
                  (lambda ()
                    (connection-process-frame conn (make-window-update-frame 0 100))))
                 +frame-data+))))))

;;; Pathname bodies are streamed, not read whole

(defun queued-octets (conn stream-id)
  "Octets held in memory for STREAM-ID's unsent response body."
  (let ((entry (gethash stream-id (woo.http2.connection:http2-connection-send-queue conn))))
    (cond
      ((null entry) 0)
      ((consp entry) (length (car entry)))
      (t (reduce #'+ (woo.http2.clack::pending-chunks entry) :key #'length)))))

(defun write-pattern-file (path size)
  (let ((bytes (make-array size :element-type '(unsigned-byte 8))))
    (dotimes (i size) (setf (aref bytes i) (mod (* i 7) 256)))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence bytes out))
    bytes))

(deftest pathname-body-streams
  (testing "a file over the send window arrives whole, held in memory a chunk at a time"
    (let* ((path (merge-pathnames "woo-h2-big.bin" (uiop:temporary-directory)))
           (size 300000)
           (opened nil)
           (conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state +state-half-closed-remote+)))
      (register-stream conn stream)
      (setf (woo.http2.connection::http2-connection-last-stream-id conn) 1)
      (unwind-protect
           (let ((expected (write-pattern-file path size))
                 (woo.http2.clack:*pathname-body-open-hook* (lambda (in) (push in opened)))
                 (received nil)
                 (max-held 0))
             (multiple-value-bind (sent frames) (capture-response conn stream 200 nil path)
               (ok (not sent) "the file does not fit in the initial window")
               (push (data-bytes frames) received)
               (ok (every (lambda (f) (<= (frame-length f) woo.http2.clack:*pathname-chunk-size*))
                          (frames-of-type frames +frame-data+))))
             (ok (= (reduce #'+ received :key #'length) 65535) "the window is filled")
             (ok opened "the file was opened")
             (ok (notany #'open-stream-p opened) "and is not held open while waiting")
             (loop repeat 50
                   while (gethash 1 (http2-connection-streams conn))
                   do (setf max-held (max max-held (queued-octets conn 1)))
                      (push (data-bytes
                             (capture-frames
                              (lambda ()
                                (connection-process-frame conn (make-window-update-frame 0 40000))
                                (connection-process-frame conn (make-window-update-frame 1 40000)))))
                            received))
             (ok (<= max-held woo.http2.clack:*pathname-chunk-size*)
                 (format nil "at most one chunk of the file in memory (held ~D)" max-held))
             (ok (equalp (concat-octets (reverse received)) expected) "every byte arrives")
             (ok (stream-closed-p stream))
             (ok (notany #'open-stream-p opened) "the file is closed when done"))
        (when (probe-file path) (delete-file path)))))

  (testing "a reset mid-file closes the file and stops sending"
    (let* ((path (merge-pathnames "woo-h2-reset.bin" (uiop:temporary-directory)))
           (opened nil)
           (conn (make-http2-connection))
           (stream (make-http2-stream :id 1 :state +state-half-closed-remote+)))
      (register-stream conn stream)
      (setf (woo.http2.connection::http2-connection-last-stream-id conn) 1)
      (unwind-protect
           (let ((woo.http2.clack:*pathname-body-open-hook* (lambda (in) (push in opened))))
             (write-pattern-file path 200000)
             (capture-response conn stream 200 nil path)
             (ok (<= (queued-octets conn 1) woo.http2.clack:*pathname-chunk-size*))
             (connection-process-frame conn (woo.http2.frames:make-rst-stream-frame
                                             1 woo.http2.constants:+cancel+))
             (ok (notany #'open-stream-p opened) "no file stream is left open")
             (ok (null (gethash 1 (woo.http2.connection:http2-connection-send-queue conn))))
             (ok (null (frames-of-type
                        (capture-frames
                         (lambda ()
                           (connection-process-frame conn (make-window-update-frame 0 100000))))
                        +frame-data+))
                 "no DATA after the reset"))
        (when (probe-file path) (delete-file path))))))

(deftest h2c-socket-responder-from-another-thread
  (let ((clack.test:*clack-test-handler* :woo))
    (clack.test:testing-app "a responder and writer used from another thread write on the loop"
        (lambda (env)
          (let ((full (string= (getf env :path-info) "/full")))
            (lambda (responder)
              (bt2:make-thread
               (lambda ()
                 (sleep 0.05)
                 (if full
                     (funcall responder '(200 (:content-type "text/plain") ("full body")))
                     (let ((writer (funcall responder '(200 (:content-type "text/plain")))))
                       (dotimes (i 5)
                         (sleep 0.02)
                         (funcall writer (format nil "c~D;" i)))
                       (funcall writer nil :close t))))
               :name "woo-test h2 responder"))))
      (let ((client (h2c-connect)))
        (unwind-protect
             (flet ((request (id path)
                      (h2c-send client
                                (make-headers-frame id (request-block
                                                        `((":method" . "GET")
                                                          (":scheme" . "http")
                                                          (":path" . ,path)
                                                          (":authority" . "localhost")))
                                                    :end-headers t :end-stream t)))
                    (ended-p (id)
                      (lambda (fs)
                        (find-if (lambda (f)
                                   (and (frame-on-stream-p f id +frame-data+)
                                        (end-stream-p f)))
                                 fs))))
               (h2c-preface-and-settings client)
               (request 1 "/stream")
               (request 3 "/full")
               (let ((frames (h2c-read-until client
                                             (lambda (fs)
                                               (and (funcall (ended-p 1) fs)
                                                    (funcall (ended-p 3) fs))))))
                 (ok (not (find woo.http2.constants:+frame-rst-stream+ frames :key #'frame-type)))
                 (ok (not (find woo.http2.constants:+frame-goaway+ frames :key #'frame-type)))
                 (ok (= 1 (length (frames-of-type (frames-on-stream frames 1) +frame-headers+))))
                 (ok (equal (map 'string #'code-char (data-bytes (frames-on-stream frames 1)))
                            "c0;c1;c2;c3;c4;")
                     "every chunk written from the other thread arrives in order")
                 (ok (equal (map 'string #'code-char (data-bytes (frames-on-stream frames 3)))
                            "full body"))))
          (usocket:socket-close (h2c-client-socket client)))))))
