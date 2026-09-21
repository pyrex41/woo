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
                :http2-connection-remote-max-frame-size
                :http2-connection-local-max-frame-size
                :http2-connection-remote-window-size)
  (:import-from :woo.http2.stream
                :make-http2-stream
                :http2-stream-window-size)
  (:import-from :woo.http2.frames
                :frame-type
                :frame-flags
                :frame-payload
                :frame-length)
  (:import-from :woo.http2.constants
                :+frame-headers+
                :+frame-continuation+
                :+frame-data+
                :+flag-end-headers+
                :+flag-end-stream+))
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
