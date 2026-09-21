(in-package :cl-user)
(defpackage woo-test.http2-connection
  (:use :cl :rove)
  (:import-from :woo.http2.connection
                :make-http2-connection
                :http2-connection
                :http2-connection-socket
                :http2-connection-streams
                :http2-connection-encoder-context
                :http2-connection-decoder-context
                :http2-connection-last-stream-id
                :http2-connection-goaway-sent
                :http2-connection-local-settings
                :http2-connection-remote-settings
                :http2-connection-window-size
                :connection-get-stream
                :connection-process-frame
                :http2-connection-remote-window-size
                :http2-connection-remote-max-frame-size
                :http2-connection-awaiting-continuation-stream-id)
  (:import-from :woo.http2.constants
                :+connection-preface+
                :+connection-preface-length+
                :+default-initial-window-size+
                :+default-max-frame-size+
                :+default-header-table-size+
                :+max-frame-size-limit+
                :+settings-header-table-size+
                :+settings-max-concurrent-streams+
                :+settings-initial-window-size+
                :+settings-max-frame-size+
                :+protocol-error+
                :+stream-closed+
                :+frame-size-error+
                :+flow-control-error+
                :+flag-end-stream+
                :+flag-end-headers+
                :+flag-padded+
                :+flag-ack+
                :+frame-headers+
                :+frame-data+
                :+frame-continuation+
                :+frame-settings+
                :+min-max-frame-size+
                :+max-frame-size-limit+
                :+max-window-size+)
  (:import-from :woo.http2.stream
                :http2-stream-id
                :http2-stream-state
                :http2-stream-window-size
                :http2-stream-recv-window-size
                :http2-stream-pending-end-stream
                :http2-stream-awaiting-continuation
                :http2-stream-headers
                :+state-idle+
                :+state-closed+
                :stream-closed-p
                :stream-half-closed-remote-p)
  (:import-from :woo.http2.frames
                :make-frame
                :make-headers-frame
                :make-data-frame
                :make-settings-frame
                :make-settings-ack-frame
                :make-window-update-frame
                :parse-frame
                :serialize-frame
                :frame-payload)
  (:import-from :woo.http2.hpack
                :hpack-context-max-dynamic-table-size))
(in-package :woo-test.http2-connection)

(deftest connection-preface-constant
  (testing "Connection preface is exactly 24 bytes"
    (ok (= +connection-preface-length+ 24)))

  (testing "Connection preface has correct value (PRI * HTTP/2.0\\r\\n\\r\\nSM\\r\\n\\r\\n)"
    (ok (equalp +connection-preface+
                (make-array 24 :element-type '(unsigned-byte 8)
                            :initial-contents '(#x50 #x52 #x49 #x20 #x2a #x20 #x48 #x54
                                                #x54 #x50 #x2f #x32 #x2e #x30 #x0d #x0a
                                                #x0d #x0a #x53 #x4d #x0d #x0a #x0d #x0a))))
    ;; Verify it's "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" by checking key bytes
    (ok (= #x50 (aref +connection-preface+ 0)) "First byte is 'P'")
    (ok (= #x52 (aref +connection-preface+ 1)) "Second byte is 'R'")
    (ok (= #x49 (aref +connection-preface+ 2)) "Third byte is 'I'")
    (ok (= #x0d (aref +connection-preface+ 14)) "Contains CR")
    (ok (= #x0a (aref +connection-preface+ 15)) "Contains LF")))

(deftest connection-creation
  (testing "make-http2-connection creates connection with initial state"
    (let ((conn (make-http2-connection)))
      (ok conn "Connection object created")

      ;; Test exported accessors
      (ok (not (http2-connection-goaway-sent conn))
          "GOAWAY not sent initially")

      ;; Test stream management
      (ok (http2-connection-streams conn)
          "Streams hash table exists")
      (ok (hash-table-p (http2-connection-streams conn))
          "Streams is a hash table")
      (ok (= 0 (hash-table-count (http2-connection-streams conn)))
          "No streams initially")

      ;; Test stream ID tracking
      (ok (= 0 (http2-connection-last-stream-id conn))
          "Last stream ID starts at 0"))))

(deftest connection-flow-control
  (testing "Connection has correct initial flow control window sizes"
    (let ((conn (make-http2-connection)))
      (ok (= +default-initial-window-size+
             (http2-connection-window-size conn))
          "Local window size is default (65535)"))))

(deftest connection-local-settings
  (testing "Connection has correct local settings"
    (let ((conn (make-http2-connection)))
      (let ((settings (http2-connection-local-settings conn)))
        (ok settings "Local settings exist")
        (ok (listp settings) "Settings is a list")

        ;; Check for required settings
        (let ((max-concurrent (cdr (assoc +settings-max-concurrent-streams+ settings)))
              (initial-window (cdr (assoc +settings-initial-window-size+ settings)))
              (max-frame-size (cdr (assoc +settings-max-frame-size+ settings)))
              (header-table-size (cdr (assoc +settings-header-table-size+ settings))))

          (ok (= 100 max-concurrent)
              "Max concurrent streams is 100")
          (ok (= +default-initial-window-size+ initial-window)
              "Initial window size matches default")
          (ok (= +default-max-frame-size+ max-frame-size)
              "Max frame size matches default")
          (ok (= +default-header-table-size+ header-table-size)
              "Header table size matches default"))))))

(deftest connection-remote-settings
  (testing "Connection starts with nil remote settings"
    (let ((conn (make-http2-connection)))
      (ok (null (http2-connection-remote-settings conn))
          "Remote settings initially nil (not received yet)"))))

(deftest connection-hpack-contexts
  (testing "Connection initializes HPACK encoder and decoder contexts"
    (let ((conn (make-http2-connection)))
      ;; Test encoder context
      (ok (http2-connection-encoder-context conn)
          "Encoder context exists")
      (ok (woo.http2.hpack::hpack-context-p
           (http2-connection-encoder-context conn))
          "Encoder is an HPACK context")
      (ok (= +default-header-table-size+
             (hpack-context-max-dynamic-table-size
              (http2-connection-encoder-context conn)))
          "Encoder has correct max table size")

      ;; Test decoder context
      (ok (http2-connection-decoder-context conn)
          "Decoder context exists")
      (ok (woo.http2.hpack::hpack-context-p
           (http2-connection-decoder-context conn))
          "Decoder is an HPACK context")
      (ok (= +default-header-table-size+
             (hpack-context-max-dynamic-table-size
              (http2-connection-decoder-context conn)))
          "Decoder has correct max table size")

      ;; Verify they are separate instances
      (ok (not (eq (http2-connection-encoder-context conn)
                   (http2-connection-decoder-context conn)))
          "Encoder and decoder are separate contexts"))))

(deftest stream-management-get-without-create
  (testing "connection-get-stream without :create returns nil for non-existent stream"
    (let ((conn (make-http2-connection)))
      (ok (null (connection-get-stream conn 1))
          "Non-existent stream returns nil")
      (ok (null (connection-get-stream conn 3))
          "Another non-existent stream returns nil")
      (ok (= 0 (hash-table-count (http2-connection-streams conn)))
          "No streams were created"))))

(deftest stream-management-get-with-create
  (testing "connection-get-stream with :create creates new stream"
    (let ((conn (make-http2-connection)))
      (let ((stream (connection-get-stream conn 1 :create t)))
        (ok stream "Stream was created")
        (ok (woo.http2.stream::http2-stream-p stream) "Result is an http2-stream")
        (ok (= 1 (http2-stream-id stream)) "Stream has correct ID")
        (ok (= +state-idle+ (http2-stream-state stream))
            "New stream is in idle state")
        (ok (= +default-initial-window-size+
               (http2-stream-window-size stream))
            "Stream has default window size")
        (ok (= 1 (hash-table-count (http2-connection-streams conn)))
            "Stream was added to connection"))))

  (testing "connection-get-stream returns existing stream on second call"
    (let ((conn (make-http2-connection)))
      (let ((stream1 (connection-get-stream conn 3 :create t))
            (stream2 (connection-get-stream conn 3)))
        (ok (eq stream1 stream2)
            "Same stream object returned")
        (ok (= 1 (hash-table-count (http2-connection-streams conn)))
            "No duplicate streams created"))))

  (testing "connection-get-stream creates multiple streams with different IDs"
    (let ((conn (make-http2-connection)))
      (let ((stream1 (connection-get-stream conn 1 :create t))
            (stream3 (connection-get-stream conn 3 :create t))
            (stream5 (connection-get-stream conn 5 :create t)))
        (ok (not (eq stream1 stream3))
            "Different stream objects")
        (ok (not (eq stream1 stream5))
            "Different stream objects")
        (ok (not (eq stream3 stream5))
            "Different stream objects")
        (ok (= 1 (http2-stream-id stream1)) "Stream 1 has ID 1")
        (ok (= 3 (http2-stream-id stream3)) "Stream 3 has ID 3")
        (ok (= 5 (http2-stream-id stream5)) "Stream 5 has ID 5")
        (ok (= 3 (hash-table-count (http2-connection-streams conn)))
            "Three streams in connection")))))

(deftest stream-management-with-callback
  (testing "connection-get-stream calls on-stream callback when creating"
    (let ((conn (make-http2-connection))
          (callback-called nil)
          (callback-stream nil))
      ;; Set the callback (internal accessor)
      (setf (woo.http2.connection::http2-connection-on-stream conn)
            (lambda (stream)
              (setf callback-called t
                    callback-stream stream)))

      ;; Create a stream
      (let ((stream (connection-get-stream conn 7 :create t)))
        (ok callback-called "Callback was invoked")
        (ok (eq stream callback-stream)
            "Callback received the correct stream"))))

  (testing "connection-get-stream doesn't call callback for existing stream"
    (let ((conn (make-http2-connection))
          (callback-count 0))
      ;; Set the callback (internal accessor)
      (setf (woo.http2.connection::http2-connection-on-stream conn)
            (lambda (stream)
              (declare (ignore stream))
              (incf callback-count)))

      ;; Create stream first time
      (connection-get-stream conn 9 :create t)
      (ok (= 1 callback-count) "Callback called once on creation")

      ;; Get existing stream
      (connection-get-stream conn 9)
      (ok (= 1 callback-count) "Callback not called for existing stream")

      ;; Try to get with :create again
      (connection-get-stream conn 9 :create t)
      (ok (= 1 callback-count) "Callback still only called once"))))

(deftest stream-window-size-inheritance
  (testing "New streams inherit window size from connection"
    (let ((conn (make-http2-connection)))
      ;; Create a new stream - it should get default window size
      (let ((stream (connection-get-stream conn 1 :create t)))
        (ok (= +default-initial-window-size+ (http2-stream-window-size stream))
            "Stream gets default window size")))))

(deftest connection-callbacks
  (testing "Connection callback slots can be set"
    (let ((conn (make-http2-connection))
          (on-stream-fn (lambda (s) (declare (ignore s)) nil)))

      (setf (woo.http2.connection::http2-connection-on-stream conn) on-stream-fn)

      (ok (eq on-stream-fn (woo.http2.connection::http2-connection-on-stream conn))
          "on-stream callback set"))))

(deftest connection-socket
  (testing "Connection socket can be set and retrieved"
    (let ((conn (make-http2-connection))
          (mock-socket "mock-socket-object"))
      (setf (http2-connection-socket conn) mock-socket)
      (ok (equal mock-socket (http2-connection-socket conn))
          "Socket value preserved"))))

(deftest constants-verification
  (testing "HTTP/2 constants have correct values"
    ;; Verify settings constants match expected values
    (ok (= 1 +settings-header-table-size+)
        "SETTINGS_HEADER_TABLE_SIZE = 0x1")
    (ok (= 3 +settings-max-concurrent-streams+)
        "SETTINGS_MAX_CONCURRENT_STREAMS = 0x3")
    (ok (= 4 +settings-initial-window-size+)
        "SETTINGS_INITIAL_WINDOW_SIZE = 0x4")
    (ok (= 5 +settings-max-frame-size+)
        "SETTINGS_MAX_FRAME_SIZE = 0x5")

    ;; Verify default values
    (ok (= 4096 +default-header-table-size+)
        "Default header table size is 4096")
    (ok (= 65535 +default-initial-window-size+)
        "Default window size is 65535")
    (ok (= 16384 +default-max-frame-size+)
        "Default frame size is 16384")
    (ok (= 16777215 +max-frame-size-limit+)
        "Max frame size limit is 2^24 - 1")))

(deftest connection-state-flags
  (testing "Connection state flags are independently settable"
    (let ((conn (make-http2-connection)))
      ;; Test goaway-sent flag (which is exported)
      (ok (not (http2-connection-goaway-sent conn))
          "GOAWAY sent initially false")

      ;; Set goaway-sent (using internal setter)
      (setf (woo.http2.connection::http2-connection-goaway-sent conn) t)
      (ok (http2-connection-goaway-sent conn)
          "GOAWAY sent flag can be set"))))

(defun test-conn (&optional extra)
  (let ((conn (apply #'make-http2-connection extra))
        (err nil))
    (setf (woo.http2.connection::http2-connection-on-error conn)
          (lambda (code debug)
            (declare (ignore debug))
            (setf err code)))
    (values conn (lambda () err))))

(deftest b4-illegal-stream-ids
  (testing "HEADERS on stream 0 is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 0 #() :end-headers t :end-stream t))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))))

  (testing "Even stream id is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 2 #() :end-headers t :end-stream t))
      (ok (= (funcall err) +protocol-error+))))

  (testing "Reused stream id is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t :end-stream t))
      (ok (null (funcall err)))
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t :end-stream t))
      (ok (= (funcall err) +protocol-error+))))

  (testing "Stream id not greater than last-stream-id is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 5 #() :end-headers t :end-stream t))
      (connection-process-frame
       conn (make-headers-frame 3 #() :end-headers t :end-stream t))
      (ok (= (funcall err) +protocol-error+))))

  (testing "Exceeding MAX_CONCURRENT_STREAMS is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (setf (woo.http2.connection::http2-connection-local-max-concurrent-streams conn) 1)
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t))
      (ok (null (funcall err)))
      (connection-process-frame
       conn (make-headers-frame 3 #() :end-headers t))
      (ok (= (funcall err) +protocol-error+)))))

(deftest b5-continuation-and-end-stream
  (testing "END_STREAM on HEADERS without END_HEADERS is preserved"
    (multiple-value-bind (conn err)
        (test-conn)
      (declare (ignore err))
      (let ((got-end nil))
        (setf (woo.http2.connection::http2-connection-on-headers conn)
              (lambda (stream headers end-stream)
                (declare (ignore stream headers))
                (setf got-end end-stream)))
        (connection-process-frame
         conn (make-headers-frame 1 #() :end-headers nil :end-stream t))
        (ok (http2-connection-awaiting-continuation-stream-id conn))
        (connection-process-frame
         conn (make-frame :type +frame-continuation+
                          :flags +flag-end-headers+
                          :stream-id 1
                          :payload #()))
        (ok got-end)
        (ok (stream-half-closed-remote-p (connection-get-stream conn 1))))))

  (testing "Non-CONTINUATION while awaiting is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers nil))
      (connection-process-frame
       conn (make-data-frame 1 #()))
      (ok (= (funcall err) +protocol-error+))))

  (testing "CONTINUATION when not awaiting is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-frame :type +frame-continuation+
                        :flags +flag-end-headers+
                        :stream-id 1
                        :payload #()))
      (ok (= (funcall err) +protocol-error+)))))

(deftest b6-padding
  (testing "PADDED HEADERS with pad-length >= payload is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-frame :type +frame-headers+
                        :flags (logior +flag-end-headers+ +flag-padded+)
                        :stream-id 1
                        :payload (make-array 1 :element-type '(unsigned-byte 8)
                                             :initial-contents '(5))))
      (ok (= (funcall err) +protocol-error+))))

  (testing "PADDED DATA with pad-length >= payload is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t))
      (connection-process-frame
       conn (make-frame :type +frame-data+
                        :flags +flag-padded+
                        :stream-id 1
                        :payload (make-array 2 :element-type '(unsigned-byte 8)
                                             :initial-contents '(10 0))))
      (ok (= (funcall err) +protocol-error+)))))

(deftest b7-settings-and-frame-size
  (testing "SETTINGS ACK with payload is FRAME_SIZE_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((ack (make-settings-ack-frame)))
        (setf (frame-payload ack)
              (make-array 6 :element-type '(unsigned-byte 8) :initial-element 0))
        (connection-process-frame conn ack)
        (ok (= (funcall err) +frame-size-error+)))))

  (testing "SETTINGS_MAX_FRAME_SIZE below 16384 is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-settings-frame (list (cons +settings-max-frame-size+ 1000))))
      (ok (= (funcall err) +protocol-error+))))

  (testing "SETTINGS_MAX_FRAME_SIZE above 2^24-1 is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-settings-frame (list (cons +settings-max-frame-size+ 16777216))))
      (ok (= (funcall err) +protocol-error+))))

  (testing "SETTINGS_INITIAL_WINDOW_SIZE overflow is FLOW_CONTROL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-settings-frame
             (list (cons +settings-initial-window-size+ #x80000000))))
      (ok (= (funcall err) +flow-control-error+))))

  (testing "parse-frame rejects advertised length over max-frame-size"
    (let ((header (make-array 9 :element-type '(unsigned-byte 8) :initial-element 0)))
      (setf (aref header 0) 0
            (aref header 1) #x40
            (aref header 2) 0)
      (multiple-value-bind (frame status)
          (parse-frame header :max-frame-size +default-max-frame-size+)
        (ok (null frame))
        (ok (eq status :frame-size-error))))))

(deftest b8-flow-control
  (testing "WINDOW_UPDATE increment 0 is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame conn (make-window-update-frame 0 0))
      (ok (= (funcall err) +protocol-error+))))

  (testing "Incoming DATA decrements recv windows and leaves send windows"
    (multiple-value-bind (conn err)
        (test-conn)
      (declare (ignore err))
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t))
      (let ((stream (connection-get-stream conn 1))
            (before-recv (http2-connection-window-size conn))
            (before-send (http2-connection-remote-window-size conn)))
        (connection-process-frame
         conn (make-data-frame 1 (make-array 10 :element-type '(unsigned-byte 8)
                                             :initial-element 1)))
        (ok (= (http2-connection-window-size conn) (- before-recv 10)))
        (ok (= (http2-stream-recv-window-size stream)
               (- +default-initial-window-size+ 10)))
        (ok (= (http2-connection-remote-window-size conn) before-send))
        (ok (= (http2-stream-window-size stream)
               (woo.http2.connection::http2-connection-remote-initial-window-size conn))))))

  (testing "DATA larger than recv window is FLOW_CONTROL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t))
      (setf (http2-connection-window-size conn) 5)
      (connection-process-frame
       conn (make-data-frame 1 (make-array 10 :element-type '(unsigned-byte 8)
                                           :initial-element 1)))
      (ok (= (funcall err) +flow-control-error+))))

  (testing "DATA after END_STREAM is STREAM_CLOSED"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t :end-stream t))
      (connection-process-frame
       conn (make-data-frame 1 (make-array 1 :element-type '(unsigned-byte 8)
                                           :initial-element 1)))
      (ok (= (funcall err) +stream-closed+))))

  (testing "Empty DATA does not apply increment 0"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 #() :end-headers t))
      (connection-process-frame conn (make-window-update-frame 1 0))
      (ok (= (funcall err) +protocol-error+)))))
