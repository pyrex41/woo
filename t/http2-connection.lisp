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
                :+settings-enable-push+
                :+settings-max-header-list-size+
                :+default-max-header-list-size+
                :+protocol-error+
                :+stream-closed+
                :+refused-stream+
                :+internal-error+
                :+frame-size-error+
                :+flow-control-error+
                :+enhance-your-calm+
                :+cancel+
                :+flag-end-stream+
                :+flag-end-headers+
                :+flag-padded+
                :+flag-ack+
                :+frame-headers+
                :+frame-data+
                :+frame-continuation+
                :+frame-settings+
                :+frame-ping+
                :+frame-goaway+
                :+frame-rst-stream+
                :+frame-push-promise+
                :+frame-window-update+
                :+min-max-frame-size+
                :+max-frame-size-limit+
                :+max-window-size+)
  (:import-from :woo.http2.stream
                :http2-stream-id
                :http2-stream-state
                :http2-stream-window-size
                :http2-stream-recv-window-size
                :http2-stream-content-length
                :http2-stream-bytes-received
                :http2-stream-pending-end-stream
                :http2-stream-awaiting-continuation
                :http2-stream-headers
                :+state-idle+
                :+state-reserved-local+
                :+state-closed+
                :stream-transition
                :stream-open-p
                :stream-closed-p
                :stream-half-closed-remote-p)
  (:import-from :woo.http2.frames
                :make-frame
                :make-headers-frame
                :make-continuation-frame
                :make-data-frame
                :make-settings-frame
                :make-settings-ack-frame
                :make-window-update-frame
                :make-ping-frame
                :make-goaway-frame
                :make-rst-stream-frame
                :make-push-promise-frame
                :parse-frame
                :parse-settings-payload
                :serialize-frame
                :frame-payload)
  (:import-from :woo.http2.hpack
                :make-hpack-context
                :hpack-encode-headers
                :hpack-context-max-dynamic-table-size))
(in-package :woo-test.http2-connection)

(defun empty-octets ()
  "Empty frame payload. A literal #() is a simple-vector, not octets."
  (make-array 0 :element-type '(unsigned-byte 8)))

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
              (header-table-size (cdr (assoc +settings-header-table-size+ settings)))
              (header-list-size (cdr (assoc +settings-max-header-list-size+ settings))))

          (ok (= 100 max-concurrent)
              "Max concurrent streams is 100")
          (ok (= +default-initial-window-size+ initial-window)
              "Initial window size matches default")
          (ok (= +default-max-frame-size+ max-frame-size)
              "Max frame size matches default")
          (ok (= +default-header-table-size+ header-table-size)
              "Header table size matches default")
          (ok (= +default-max-header-list-size+ header-list-size)
              "SETTINGS_MAX_HEADER_LIST_SIZE is advertised"))))))

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
       conn (make-headers-frame 0 (empty-octets) :end-headers t :end-stream t))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))))

  (testing "Even stream id is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 2 (empty-octets) :end-headers t :end-stream t))
      (ok (= (funcall err) +protocol-error+))))

  (testing "HEADERS after the remote half-close is RST STREAM_CLOSED"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (ok (null (funcall err)))
      (ok (stream-half-closed-remote-p (connection-get-stream conn 1)))
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (ok (= (funcall err) +stream-closed+))
      (ok (not (http2-connection-goaway-sent conn)))
      (ok (equal (woo.http2.connection::http2-connection-last-rst conn)
                 (cons 1 +stream-closed+)))
      (ok (stream-closed-p (connection-get-stream conn 1)))))

  (testing "Stream id not greater than last-stream-id is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 5 (empty-octets) :end-headers t :end-stream t))
      (connection-process-frame
       conn (make-headers-frame 3 (empty-octets) :end-headers t :end-stream t))
      (ok (= (funcall err) +protocol-error+))))

  (testing "Exceeding MAX_CONCURRENT_STREAMS is RST REFUSED_STREAM"
    (multiple-value-bind (conn err)
        (test-conn)
      (setf (woo.http2.connection::http2-connection-local-max-concurrent-streams conn) 1)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (ok (null (funcall err)))
      (connection-process-frame
       conn (make-headers-frame 3 (empty-octets) :end-headers t))
      (ok (= (funcall err) +refused-stream+))
      (ok (equal (last-rst conn) (cons 3 +refused-stream+)))
      (ok (not (http2-connection-goaway-sent conn)))))

  (testing "A refused HEADERS block is still applied to the dynamic table"
    (multiple-value-bind (conn err)
        (test-conn)
      (setf (woo.http2.connection::http2-connection-local-max-concurrent-streams conn) 1)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (ok (null (funcall err)))
      (let ((block (make-array 26 :element-type '(unsigned-byte 8)
                               :initial-contents
                               '(#x40 #x0a #x63 #x75 #x73 #x74 #x6f #x6d #x2d #x6b #x65 #x79
                                 #x0d #x63 #x75 #x73 #x74 #x6f #x6d #x2d #x68 #x65 #x61 #x64 #x65 #x72))))
        (connection-process-frame
         conn (make-headers-frame 3 block :end-headers t :end-stream t)))
      (ok (= (funcall err) +refused-stream+))
      (ok (not (http2-connection-goaway-sent conn)))
      (let ((open (connection-get-stream conn 1)))
        (stream-transition open :send-end-stream)
        (woo.http2.connection::connection-drop-closed-stream conn open))
      (let ((got nil))
        (setf (woo.http2.connection::http2-connection-on-headers conn)
              (lambda (stream headers end-stream)
                (declare (ignore stream end-stream))
                (setf got headers)))
        (connection-process-frame
         conn (make-headers-frame
               5
               (make-array 1 :element-type '(unsigned-byte 8) :initial-element #xbe)
               :end-headers t :end-stream t))
        (ok (not (http2-connection-goaway-sent conn))
            "indexed lookup of the refused block is not a compression error")
        (ok (equal got '(("custom-key" . "custom-header"))))))))

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
         conn (make-headers-frame 1 (empty-octets) :end-headers nil :end-stream t))
        (ok (http2-connection-awaiting-continuation-stream-id conn))
        (connection-process-frame
         conn (make-frame :type +frame-continuation+
                          :flags +flag-end-headers+
                          :stream-id 1
                          :payload (empty-octets)))
        (ok got-end)
        (ok (stream-half-closed-remote-p (connection-get-stream conn 1))))))

  (testing "Non-CONTINUATION while awaiting is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers nil))
      (connection-process-frame
       conn (make-data-frame 1 (empty-octets)))
      (ok (= (funcall err) +protocol-error+))))

  (testing "CONTINUATION when not awaiting is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-frame :type +frame-continuation+
                        :flags +flag-end-headers+
                        :stream-id 1
                        :payload (empty-octets)))
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
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
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
      ;; 16385: one octet over the default. 16384 itself is legal.
      (setf (aref header 0) 0
            (aref header 1) #x40
            (aref header 2) 1)
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
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
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
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (setf (http2-connection-window-size conn) 5)
      (connection-process-frame
       conn (make-data-frame 1 (make-array 10 :element-type '(unsigned-byte 8)
                                           :initial-element 1)))
      (ok (= (funcall err) +flow-control-error+))
      (ok (= (http2-connection-window-size conn) 5)
          "overflow does not debit the recv window")
      (ok (= (http2-connection-remote-window-size conn)
             +default-initial-window-size+)
          "overflow does not touch the send window")
      (ok (http2-connection-goaway-sent conn)
          "flow-control overflow is a connection error")
      (ok (null (woo.http2.connection::http2-connection-last-rst conn))
          "flow-control overflow is not a stream RST")))

  (testing "DATA after END_STREAM is STREAM_CLOSED"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (connection-process-frame
       conn (make-data-frame 1 (make-array 1 :element-type '(unsigned-byte 8)
                                           :initial-element 1)))
      (ok (= (funcall err) +stream-closed+))
      (ok (not (http2-connection-goaway-sent conn))
          "DATA after END_STREAM is RST, not GOAWAY")
      (ok (equal (woo.http2.connection::http2-connection-last-rst conn)
                 (cons 1 +stream-closed+)))
      (ok (= (http2-connection-window-size conn)
             (- +default-initial-window-size+ 1))
          "a fitting DATA frame debits the connection window")
      (ok (stream-closed-p (connection-get-stream conn 1)))))

  (testing "DATA after END_STREAM that exceeds the connection window is FLOW_CONTROL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (setf (http2-connection-window-size conn) 5)
      (connection-process-frame
       conn (make-data-frame 1 (make-array 10 :element-type '(unsigned-byte 8)
                                           :initial-element 1)))
      (ok (= (funcall err) +flow-control-error+))
      (ok (http2-connection-goaway-sent conn))
      (ok (null (woo.http2.connection::http2-connection-last-rst conn)))
      (ok (= (http2-connection-window-size conn) 5))))

  (testing "Empty DATA does not apply increment 0"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (connection-process-frame conn (make-window-update-frame 1 0))
      (ok (= (funcall err) +protocol-error+)))))

(deftest b4-trailers-and-state-errors
  (testing "HEADERS in open with END_STREAM are trailers and half-close remote"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((ends nil))
        (setf (woo.http2.connection::http2-connection-on-headers conn)
              (lambda (stream headers end-stream)
                (declare (ignore stream headers))
                (push end-stream ends)))
        (connection-process-frame
         conn (make-headers-frame 1 (empty-octets) :end-headers t))
        (ok (stream-open-p (connection-get-stream conn 1)))
        (connection-process-frame
         conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (null (woo.http2.connection::http2-connection-last-rst conn)))
        (ok (stream-half-closed-remote-p (connection-get-stream conn 1)))
        (ok (equal (reverse ends) '(nil t))))))

  (testing "HEADERS on reserved-local is GOAWAY PROTOCOL_ERROR, not INTERNAL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((stream (connection-get-stream conn 1 :create t)))
        (stream-transition stream :send-push-promise)
        (ok (= (http2-stream-state stream) +state-reserved-local+)))
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (ok (= (funcall err) +protocol-error+))
      (ok (/= (funcall err) +internal-error+))
      (ok (http2-connection-goaway-sent conn))
      (ok (null (woo.http2.connection::http2-connection-last-rst conn)))))

  (testing "HEADERS on half-closed-remote is RST STREAM_CLOSED, not GOAWAY"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (ok (stream-half-closed-remote-p (connection-get-stream conn 1)))
      ;; custom-key: custom-header with incremental indexing (RFC 7541 C.2.1).
      ;; The block must reach the dynamic table even though the stream is RST.
      (connection-process-frame
       conn (make-headers-frame 1 (custom-header-literal) :end-headers t))
      (ok (= (funcall err) +stream-closed+))
      (ok (/= (funcall err) +internal-error+))
      (ok (not (http2-connection-goaway-sent conn)))
      (ok (equal (woo.http2.connection::http2-connection-last-rst conn)
                 (cons 1 +stream-closed+)))
      (ok (stream-closed-p (connection-get-stream conn 1)))
      (let ((got nil))
        (setf (woo.http2.connection::http2-connection-on-headers conn)
              (lambda (stream headers end-stream)
                (declare (ignore stream end-stream))
                (setf got headers)))
        ;; #xbe is dynamic index 62: the entry the RST block inserted.
        (connection-process-frame
         conn (make-headers-frame 3 (make-array 1 :element-type '(unsigned-byte 8)
                                                  :initial-element #xbe)
                                  :end-headers t :end-stream t))
        (ok (not (http2-connection-goaway-sent conn))
            "the RST stream's block was decoded into the dynamic table")
        (ok (equal got '(("custom-key" . "custom-header"))))))))

(defun custom-header-literal ()
  (make-array 26 :element-type '(unsigned-byte 8)
                 :initial-contents
                 '(#x40 #x0a #x63 #x75 #x73 #x74 #x6f #x6d #x2d #x6b #x65 #x79
                   #x0d #x63 #x75 #x73 #x74 #x6f #x6d #x2d #x68 #x65 #x61 #x64 #x65 #x72)))

(defun ub8 (n &optional (byte 0))
  (make-array n :element-type '(unsigned-byte 8) :initial-element byte))

(defun hpack-block (headers)
  (hpack-encode-headers (make-hpack-context) headers))

(defun headers-of (stream-id headers &key end-stream (end-headers t))
  (make-headers-frame stream-id (hpack-block headers)
                      :end-stream end-stream
                      :end-headers end-headers))

(defun set-header-list-limit (conn n)
  "Edit a copy, so no other connection's settings can change."
  (let ((settings (copy-alist (http2-connection-local-settings conn))))
    (setf (cdr (assoc +settings-max-header-list-size+ settings)) n
          (http2-connection-local-settings conn) settings)
    n))

(defun concat-octets (&rest parts)
  (let* ((len (reduce #'+ parts :key #'length :initial-value 0))
         (out (make-array len :element-type '(unsigned-byte 8)))
         (start 0))
    (dolist (part parts out)
      (replace out part :start1 start)
      (incf start (length part)))))

(defun parse-bytes (conn bytes)
  (woo.http2.connection::parse-connection-data conn bytes 0 (length bytes)))

(defun last-rst (conn)
  (woo.http2.connection::http2-connection-last-rst conn))

(deftest content-length-checks
  (testing "non-integer content-length is RST PROTOCOL_ERROR and the connection stays up"
    (dolist (bad '("nope" "" "4 " " 4" "-1" "+4" "4.0" "4,4" "4, 4"))
      (multiple-value-bind (conn err)
          (test-conn)
        (let ((headers-called nil))
          (setf (woo.http2.connection::http2-connection-on-headers conn)
                (lambda (stream headers end-stream)
                  (declare (ignore stream headers end-stream))
                  (setf headers-called t)))
          (connection-process-frame
           conn (headers-of 1 `(("content-length" . ,bad)) :end-stream t))
          (ok (not headers-called) bad)
          (ok (= (funcall err) +protocol-error+) bad)
          (ok (/= (funcall err) +internal-error+) bad)
          (ok (not (http2-connection-goaway-sent conn)) bad)
          (ok (equal (last-rst conn) (cons 1 +protocol-error+)) bad)
          (ok (stream-closed-p (connection-get-stream conn 1)) bad)
          (connection-process-frame
           conn (make-headers-frame 3 (empty-octets) :end-headers t))
          (ok (not (http2-connection-goaway-sent conn)) bad)
          (ok (stream-open-p (connection-get-stream conn 3)) bad)))))

  (testing "duplicate content-length values are RST PROTOCOL_ERROR"
    (dolist (values '(("4" "4") ("4" "5")))
      (multiple-value-bind (conn err)
          (test-conn)
        (connection-process-frame
         conn (headers-of 1 (mapcar (lambda (v) (cons "content-length" v)) values)
                          :end-stream t))
        (ok (= (funcall err) +protocol-error+))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (equal (last-rst conn) (cons 1 +protocol-error+))))))

  (testing "a later content-length is a duplicate even when it matches"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((n 0))
        (setf (woo.http2.connection::http2-connection-on-headers conn)
              (lambda (stream headers end-stream)
                (declare (ignore stream headers end-stream))
                (incf n)))
        (connection-process-frame
         conn (headers-of 1 '(("content-length" . "4"))))
        (ok (= n 1))
        (connection-process-frame
         conn (make-data-frame 1 (ub8 4 97)))
        (connection-process-frame
         conn (headers-of 1 '(("content-length" . "4")) :end-stream t))
        (ok (= n 1))
        (ok (= (funcall err) +protocol-error+))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (equal (last-rst conn) (cons 1 +protocol-error+))))))

  (testing "content-length 0 with END_STREAM and no body is valid"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((saw nil))
        (setf (woo.http2.connection::http2-connection-on-headers conn)
              (lambda (stream headers end-stream)
                (declare (ignore headers))
                (setf saw (list (http2-stream-content-length stream) end-stream))))
        (connection-process-frame
         conn (headers-of 1 '(("content-length" . "0")) :end-stream t))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (equal saw '(0 t)))
        (ok (stream-half-closed-remote-p (connection-get-stream conn 1)))
        (ok (= 0 (http2-stream-bytes-received (connection-get-stream conn 1)))))))

  (testing "content-length 0 then a body byte is RST PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((data-called nil))
        (setf (woo.http2.connection::http2-connection-on-data conn)
              (lambda (stream data end-stream)
                (declare (ignore stream data end-stream))
                (setf data-called t)))
        (connection-process-frame
         conn (headers-of 1 '(("content-length" . "0"))))
        (connection-process-frame
         conn (make-data-frame 1 (ub8 1 97) :end-stream t))
        (ok (not data-called))
        (ok (= (funcall err) +protocol-error+))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (equal (last-rst conn) (cons 1 +protocol-error+))))))

  (testing "END_STREAM shorter than content-length is RST and not delivered"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((data-called nil))
        (setf (woo.http2.connection::http2-connection-on-data conn)
              (lambda (stream data end-stream)
                (declare (ignore stream data end-stream))
                (setf data-called t)))
        (connection-process-frame
         conn (headers-of 1 '(("content-length" . "4"))))
        (connection-process-frame
         conn (make-data-frame 1 (ub8 3 97) :end-stream t))
        (ok (not data-called))
        (ok (= (funcall err) +protocol-error+))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (stream-closed-p (connection-get-stream conn 1))))))

  (testing "a body longer than content-length is RST before END_STREAM"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((chunks 0))
        (setf (woo.http2.connection::http2-connection-on-data conn)
              (lambda (stream data end-stream)
                (declare (ignore stream data end-stream))
                (incf chunks)))
        (connection-process-frame
         conn (headers-of 1 '(("content-length" . "4"))))
        (connection-process-frame
         conn (make-data-frame 1 (ub8 2 97)))
        (ok (null (funcall err)))
        (ok (= chunks 1))
        (connection-process-frame
         conn (make-data-frame 1 (ub8 3 97)))
        (ok (= chunks 1))
        (ok (= (funcall err) +protocol-error+))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (equal (last-rst conn) (cons 1 +protocol-error+))))))

  (testing "bytes-received must equal content-length at END_STREAM"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((chunks 0))
        (setf (woo.http2.connection::http2-connection-on-data conn)
              (lambda (stream data end-stream)
                (declare (ignore stream data end-stream))
                (incf chunks)))
        (connection-process-frame
         conn (headers-of 1 '(("content-length" . "4"))))
        (connection-process-frame
         conn (make-data-frame 1 (ub8 2 97)))
        (connection-process-frame
         conn (make-data-frame 1 (ub8 2 98) :end-stream t))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (= chunks 2))
        (let ((stream (connection-get-stream conn 1)))
          (ok (= 4 (http2-stream-content-length stream)))
          (ok (= 4 (http2-stream-bytes-received stream)))
          (ok (stream-half-closed-remote-p stream)))))))

(deftest ignored-frames-are-connection-errors
  (testing "client PUSH_PROMISE is GOAWAY PROTOCOL_ERROR and stops the connection"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-push-promise-frame 1 2 (ub8 0)))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))
      (ok (null (last-rst conn)))
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (ok (null (connection-get-stream conn 1)))))

  (testing "PING with a non-zero stream id is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-frame :type +frame-ping+ :flags 0 :stream-id 1 :payload (ub8 8)))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))
      (ok (null (last-rst conn)))))

  (testing "PING whose payload is not 8 octets is FRAME_SIZE_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-frame :type +frame-ping+ :flags 0 :stream-id 0 :payload (ub8 3)))
      (ok (= (funcall err) +frame-size-error+))
      (ok (http2-connection-goaway-sent conn))))

  (testing "a valid PING is not an error"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame conn (make-ping-frame (ub8 8 7)))
      (ok (null (funcall err)))
      (ok (not (http2-connection-goaway-sent conn)))))

  (testing "WINDOW_UPDATE on an idle stream is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame conn (make-window-update-frame 1 1000))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))
      (ok (null (last-rst conn)))
      (ok (= (http2-connection-remote-window-size conn)
             +default-initial-window-size+))
      (ok (= (http2-connection-window-size conn)
             +default-initial-window-size+))))

  (testing "WINDOW_UPDATE on an explicitly idle stream is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-get-stream conn 1 :create t)
      (connection-process-frame conn (make-window-update-frame 1 50))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))))

  (testing "WINDOW_UPDATE on stream 0 credits only the send window"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((recv (http2-connection-window-size conn)))
        (connection-process-frame conn (make-window-update-frame 0 1000))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (= (http2-connection-remote-window-size conn)
               (+ +default-initial-window-size+ 1000)))
        (ok (= (http2-connection-window-size conn) recv)))))

  (testing "WINDOW_UPDATE on an open stream credits the stream send window"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (let ((stream (connection-get-stream conn 1))
            (recv (http2-stream-recv-window-size
                   (connection-get-stream conn 1))))
        (connection-process-frame conn (make-window-update-frame 1 25))
        (ok (null (funcall err)))
        (ok (= (http2-stream-window-size stream)
               (+ +default-initial-window-size+ 25)))
        (ok (= (http2-stream-recv-window-size stream) recv)))))

  (testing "WINDOW_UPDATE on half-closed (remote) is not an error"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (let ((stream (connection-get-stream conn 1)))
        (connection-process-frame conn (make-window-update-frame 1 10))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (null (last-rst conn)))
        (ok (stream-half-closed-remote-p stream))
        (ok (= (http2-stream-window-size stream)
               (+ +default-initial-window-size+ 10))))))

  (testing "RST_STREAM on stream 0 is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame conn (make-rst-stream-frame 0 +cancel+))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))
      (ok (null (last-rst conn)))))

  (testing "RST_STREAM on an idle stream is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame conn (make-rst-stream-frame 1 +cancel+))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))))

  (testing "RST_STREAM payload other than 4 octets is FRAME_SIZE_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-frame :type +frame-rst-stream+ :flags 0 :stream-id 1
                        :payload (ub8 2)))
      (ok (= (funcall err) +frame-size-error+))
      (ok (http2-connection-goaway-sent conn))))

  (testing "RST_STREAM on an open stream closes it and leaves the connection up"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (connection-process-frame conn (make-rst-stream-frame 1 +cancel+))
      (ok (null (funcall err)))
      (ok (not (http2-connection-goaway-sent conn)))
      (ok (null (last-rst conn)))
      (ok (stream-closed-p (connection-get-stream conn 1)))
      (connection-process-frame conn (make-rst-stream-frame 1 +cancel+))
      (ok (null (funcall err)))
      (ok (not (http2-connection-goaway-sent conn)))))

  (testing "GOAWAY on a non-zero stream is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((called nil))
        (setf (woo.http2.connection::http2-connection-on-goaway conn)
              (lambda (last code debug)
                (declare (ignore last code debug))
                (setf called t)))
        (connection-process-frame
         conn (make-frame :type +frame-goaway+ :flags 0 :stream-id 1
                          :payload (ub8 8)))
        (ok (not called))
        (ok (= (funcall err) +protocol-error+))
        (ok (http2-connection-goaway-sent conn))
        (ok (not (woo.http2.connection::http2-connection-goaway-received conn))))))

  (testing "GOAWAY shorter than 8 octets is FRAME_SIZE_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-frame :type +frame-goaway+ :flags 0 :stream-id 0
                        :payload (ub8 7)))
      (ok (= (funcall err) +frame-size-error+))
      (ok (http2-connection-goaway-sent conn))))

  (testing "a valid GOAWAY is delivered and is not a connection error"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((got nil))
        (setf (woo.http2.connection::http2-connection-on-goaway conn)
              (lambda (last code debug)
                (declare (ignore debug))
                (setf got (list last code))))
        (connection-process-frame
         conn (make-goaway-frame 7 +protocol-error+))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (woo.http2.connection::http2-connection-goaway-received conn))
        (ok (equal got (list 7 +protocol-error+))))))

  (testing "SETTINGS_ENABLE_PUSH other than 0 or 1 is PROTOCOL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-settings-frame (list (cons +settings-enable-push+ 2))))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))
      (ok (null (assoc +settings-enable-push+
                       (http2-connection-remote-settings conn))))))

  (testing "SETTINGS_ENABLE_PUSH 0 and 1 are accepted"
    (dolist (value '(0 1))
      (multiple-value-bind (conn err)
          (test-conn)
        (connection-process-frame
         conn (make-settings-frame (list (cons +settings-enable-push+ value))))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (= value (cdr (assoc +settings-enable-push+
                                 (http2-connection-remote-settings conn))))))))

  (testing "an unknown frame type is still ignored"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-frame :type 20 :flags 0 :stream-id 0 :payload (ub8 0)))
      (ok (null (funcall err)))
      (ok (not (http2-connection-goaway-sent conn)))))

  (testing "the first frame after the preface must be SETTINGS"
    (multiple-value-bind (conn err)
        (test-conn)
      (parse-bytes conn
                   (concat-octets
                    +connection-preface+
                    (serialize-frame (make-frame :type +frame-ping+
                                                 :flags 0 :stream-id 0
                                                 :payload (ub8 8)))
                    (serialize-frame (make-headers-frame 1 (empty-octets)
                                                        :end-headers t
                                                        :end-stream t))))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))
      (ok (null (connection-get-stream conn 1)))
      (parse-bytes conn (serialize-frame (make-headers-frame 3 (empty-octets) :end-headers t)))
      (ok (null (connection-get-stream conn 3)))))

  (testing "SETTINGS ACK is not a valid first frame"
    (multiple-value-bind (conn err)
        (test-conn)
      (parse-bytes conn
                   (concat-octets +connection-preface+
                                  (serialize-frame (make-settings-ack-frame))))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))))

  (testing "preface followed by SETTINGS then other frames is accepted"
    (multiple-value-bind (conn err)
        (test-conn)
      (parse-bytes conn
                   (concat-octets
                    +connection-preface+
                    (serialize-frame (make-settings-frame nil))
                    (serialize-frame (make-ping-frame (ub8 8)))
                    (serialize-frame (make-headers-frame 1 (empty-octets) :end-headers t))))
      (ok (null (funcall err)))
      (ok (not (http2-connection-goaway-sent conn)))
      (ok (woo.http2.connection::http2-connection-preface-received conn))
      (ok (not (woo.http2.connection::http2-connection-awaiting-first-settings conn)))
      (ok (stream-open-p (connection-get-stream conn 1))))))

(deftest data-after-remote-close-is-stream-error
  (testing "DATA on a fully closed stream is RST STREAM_CLOSED, not GOAWAY"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (let ((stream (connection-get-stream conn 1)))
        (stream-transition stream :send-end-stream)
        (ok (stream-closed-p stream))
        (let ((recv (http2-connection-window-size conn))
              (send (http2-connection-remote-window-size conn)))
          (connection-process-frame
           conn (make-data-frame 1 (ub8 4 1)))
          (ok (= (funcall err) +stream-closed+))
          (ok (not (http2-connection-goaway-sent conn)))
          (ok (equal (last-rst conn) (cons 1 +stream-closed+)))
          (ok (= (http2-connection-window-size conn) (- recv 4))
              "a fitting DATA frame debits the connection window")
          (ok (= (http2-connection-remote-window-size conn) send))
          (connection-process-frame
           conn (make-headers-frame 3 (empty-octets) :end-headers t))
          (ok (not (http2-connection-goaway-sent conn)))
          (ok (stream-open-p (connection-get-stream conn 3))))))))

(deftest header-list-size-limit
  (testing "SETTINGS_MAX_HEADER_LIST_SIZE is on the advertised SETTINGS frame"
    (let* ((conn (make-http2-connection))
           (frame (make-settings-frame (http2-connection-local-settings conn)))
           (parsed (parse-settings-payload (frame-payload frame))))
      (ok (= +settings-max-header-list-size+ #x6))
      (ok (= +default-max-header-list-size+
             (cdr (assoc +settings-max-header-list-size+ parsed))))))

  (testing "a peer SETTINGS_MAX_HEADER_LIST_SIZE is accepted"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame
       conn (make-settings-frame (list (cons +settings-max-header-list-size+ 1000))))
      (ok (null (funcall err)))
      (ok (not (http2-connection-goaway-sent conn)))
      (ok (= 1000 (cdr (assoc +settings-max-header-list-size+
                              (http2-connection-remote-settings conn)))))))

  (testing "a header block over the cap is GOAWAY ENHANCE_YOUR_CALM before decode"
    (multiple-value-bind (conn err)
        (test-conn)
      (set-header-list-limit conn 10)
      (connection-process-frame
       conn (make-headers-frame 1 (ub8 20 #xff) :end-headers t))
      (ok (= (funcall err) +enhance-your-calm+))
      (ok (http2-connection-goaway-sent conn))
      (ok (null (last-rst conn)))
      (ok (null (connection-get-stream conn 1)))))

  (testing "CONTINUATION concatenation is capped before the block is complete"
    (multiple-value-bind (conn err)
        (test-conn)
      (set-header-list-limit conn 8)
      (connection-process-frame
       conn (make-headers-frame 1 (ub8 4) :end-headers nil))
      (ok (null (funcall err)))
      (ok (= 1 (http2-connection-awaiting-continuation-stream-id conn)))
      (connection-process-frame
       conn (make-continuation-frame 1 (ub8 5) :end-headers nil))
      (ok (= (funcall err) +enhance-your-calm+))
      (ok (http2-connection-goaway-sent conn))
      (ok (null (http2-connection-awaiting-continuation-stream-id conn)))
      (connection-process-frame
       conn (make-headers-frame 3 (empty-octets) :end-headers t))
      (ok (null (connection-get-stream conn 3)))))

  (testing "uncompressed header list at the cap is accepted and one octet over is not"
    (let ((size (woo.http2.connection::uncompressed-header-list-size
                 '(("a" . "b")))))
      (ok (= size 34))
      (multiple-value-bind (conn err)
          (test-conn)
        (set-header-list-limit conn size)
        (connection-process-frame
         conn (headers-of 1 '(("a" . "b"))))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (stream-open-p (connection-get-stream conn 1))))
      (multiple-value-bind (conn err)
          (test-conn)
        (let ((called nil))
          (setf (woo.http2.connection::http2-connection-on-headers conn)
                (lambda (stream headers end-stream)
                  (declare (ignore stream headers end-stream))
                  (setf called t)))
          (set-header-list-limit conn (1- size))
          (connection-process-frame
           conn (headers-of 1 '(("a" . "b"))))
          (ok (not called))
          (ok (= (funcall err) +enhance-your-calm+))
          (ok (http2-connection-goaway-sent conn))
          (ok (null (last-rst conn)))
          (let ((stream (connection-get-stream conn 1)))
            (ok stream)
            (ok (= +state-idle+ (http2-stream-state stream)))))))))

;;; Frames the connection writes, captured through *http2-frame-sink*.

(defmacro with-sent-frames ((var) &body body)
  "Bind VAR to a function returning the frames sent so far, oldest first."
  (let ((frames (gensym "FRAMES")))
    `(let* ((,frames nil)
            (woo.http2.connection:*http2-frame-sink*
              (lambda (frame) (push frame ,frames))))
       (flet ((,var () (reverse ,frames)))
         ,@body))))

(defun window-updates (frames stream-id)
  "Increments of the WINDOW_UPDATE frames for STREAM-ID, in order."
  (loop for frame in frames
        when (and (= (woo.http2.frames:frame-type frame) +frame-window-update+)
                  (= (woo.http2.frames:frame-stream-id frame) stream-id))
          collect (woo.http2.frames:parse-window-update-payload
                   (frame-payload frame))))

(defun padded-data-frame (stream-id data pad-length &key end-stream)
  (let ((payload (make-array (+ 1 (length data) pad-length)
                             :element-type '(unsigned-byte 8)
                             :initial-element 0)))
    (setf (aref payload 0) pad-length)
    (replace payload data :start1 1)
    (make-frame :type +frame-data+
                :flags (logior +flag-padded+ (if end-stream +flag-end-stream+ 0))
                :stream-id stream-id
                :payload payload)))

(deftest receive-window-replenishment
  (testing "more than 64 KB on one stream is credited back with WINDOW_UPDATE"
    (multiple-value-bind (conn err)
        (test-conn)
      (with-sent-frames (sent)
        (connection-process-frame
         conn (make-headers-frame 1 (empty-octets) :end-headers t))
        (let ((stream (connection-get-stream conn 1))
              (raw 0))
          ;; 5 x 16384 plus a padded frame: 98,561 octets on the wire.
          (dotimes (i 5)
            (connection-process-frame conn (make-data-frame 1 (ub8 16384 7)))
            (incf raw 16384))
          (connection-process-frame conn (padded-data-frame 1 (ub8 1000 7) 200))
          (incf raw (+ 1 1000 200))
          (ok (null (funcall err)) "no FLOW_CONTROL_ERROR past the initial window")
          (ok (not (http2-connection-goaway-sent conn)))
          (ok (= (http2-stream-bytes-received stream) (+ (* 5 16384) 1000))
              "padding is not body")
          (let ((conn-inc (window-updates (sent) 0))
                (stream-inc (window-updates (sent) 1)))
            (ok conn-inc "connection WINDOW_UPDATE sent")
            (ok stream-inc "stream WINDOW_UPDATE sent")
            (ok (every #'plusp (append conn-inc stream-inc)))
            ;; window = initial - consumed (padding included) + credited
            (ok (= (http2-connection-window-size conn)
                   (+ (- +default-initial-window-size+ raw)
                      (reduce #'+ conn-inc))))
            (ok (= (http2-stream-recv-window-size stream)
                   (+ (- +default-initial-window-size+ raw)
                      (reduce #'+ stream-inc))))
            (ok (> (http2-connection-window-size conn)
                   (floor +default-initial-window-size+ 2)))
            ;; The send windows are separate and untouched.
            (ok (= (http2-connection-remote-window-size conn)
                   +default-initial-window-size+))))
        ;; A frame that ends the stream credits the connection, not the stream.
        (let ((before (length (window-updates (sent) 1)))
              (stream (connection-get-stream conn 1)))
          (setf (http2-connection-window-size conn) 40000
                (woo.http2.stream:http2-stream-recv-window-size stream) 40000)
          (connection-process-frame
           conn (make-data-frame 1 (ub8 10000 7) :end-stream t))
          (ok (null (funcall err)))
          (ok (stream-half-closed-remote-p stream))
          (ok (= (length (window-updates (sent) 1)) before)
              "no stream credit once the peer has ended the stream")
          (ok (= (car (last (window-updates (sent) 0))) 35535)
              "connection credit restores the initial window")))))

  (testing "more than 64 KB across streams needs connection credit"
    (multiple-value-bind (conn err)
        (test-conn)
      (with-sent-frames (sent)
        (dolist (id '(1 3 5))
          (connection-process-frame
           conn (make-headers-frame id (empty-octets) :end-headers t))
          (connection-process-frame conn (make-data-frame id (ub8 15000 1)))
          (connection-process-frame
           conn (make-data-frame id (ub8 15000 1) :end-stream t)))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (window-updates (sent) 0))
        (ok (= (http2-connection-window-size conn)
               (+ (- +default-initial-window-size+ 90000)
                  (reduce #'+ (window-updates (sent) 0)))))
        (ok (null (window-updates (sent) 1))
            "30,000 octets per stream never reaches the stream threshold"))))

  (testing "DATA on a closed stream still returns connection credit"
    (multiple-value-bind (conn err)
        (test-conn)
      (with-sent-frames (sent)
        (connection-process-frame
         conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
        (dotimes (i 5)
          (connection-process-frame conn (make-data-frame 1 (ub8 16384 1))))
        (ok (= (funcall err) +stream-closed+))
        (ok (not (http2-connection-goaway-sent conn))
            "STREAM_CLOSED RSTs, the connection window never runs out")
        (ok (window-updates (sent) 0))))))

(defun many-indexed-refs-block (value-length refs)
  "Literal x: <VALUE-LENGTH a's> with incremental indexing, then REFS
   indexed references (#xbe, dynamic index 62) to that entry."
  (let* ((head (concat-octets (make-array 3 :element-type '(unsigned-byte 8)
                                            :initial-contents '(#x40 #x01 #x78))
                              (let ((len (woo.http2.hpack::hpack-encode-integer
                                          value-length 7 0)))
                                (make-array (length len) :element-type '(unsigned-byte 8)
                                                         :initial-contents len))
                              (ub8 value-length 97))))
    (concat-octets head (ub8 refs #xbe))))

(deftest header-list-size-enforced-while-decoding
  (testing "indexed references to a large entry stop at the cap"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((called nil)
            ;; #x80 is indexed field 0, an HPACK error. Decoding reaches it
            ;; only if the block is decoded past the cap and checked after,
            ;; which would be COMPRESSION_ERROR instead of ENHANCE_YOUR_CALM.
            (block (concat-octets (many-indexed-refs-block 4000 1000)
                                  (ub8 1 #x80))))
        (setf (woo.http2.connection::http2-connection-on-headers conn)
              (lambda (stream headers end-stream)
                (declare (ignore stream headers end-stream))
                (setf called t)))
        (ok (< (length block) +default-max-header-list-size+)
            "the compressed block passes the pre-decode size check")
        (connection-process-frame
         conn (make-headers-frame 1 block :end-headers t :end-stream t))
        (ok (= (funcall err) +enhance-your-calm+)
            "decoding stopped at the cap, before the poisoned tail")
        (ok (http2-connection-goaway-sent conn))
        (ok (not called))))))

(deftest refused-stream-code
  (testing "a refused stream's block is decoded, then RST REFUSED_STREAM"
    (multiple-value-bind (conn err)
        (test-conn)
      (setf (woo.http2.connection::http2-connection-local-max-concurrent-streams conn) 1)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (with-sent-frames (sent)
        (connection-process-frame
         conn (make-headers-frame 3 (custom-header-literal) :end-headers t))
        (let ((rst (find +frame-rst-stream+ (sent) :key #'woo.http2.frames:frame-type)))
          (ok rst)
          (ok (= (woo.http2.frames:frame-stream-id rst) 3))
          (ok (= (woo.http2.frames:parse-rst-stream-payload (frame-payload rst))
                 +refused-stream+))))
      (ok (= (funcall err) +refused-stream+))
      (ok (not (http2-connection-goaway-sent conn))))))

(defun complete-stream (conn id)
  "Open stream ID with a finished request, then finish our side."
  (connection-process-frame
   conn (make-headers-frame id (empty-octets) :end-headers t :end-stream t))
  (let ((stream (connection-get-stream conn id)))
    (stream-transition stream :send-end-stream)
    (woo.http2.connection::connection-drop-closed-stream conn stream)))

(deftest closed-streams-are-bounded
  (testing "1000 completed streams keep a bounded id table and no stream objects"
    (multiple-value-bind (conn err)
        (test-conn)
      (loop for id from 1 below 2000 by 2
            do (complete-stream conn id))
      (let ((closed (woo.http2.connection::http2-connection-closed-streams conn)))
        (ok (null (funcall err)))
        ;; A literal: raising the retention default must fail this test.
        (ok (<= (hash-table-count closed) 128))
        (ok (plusp (hash-table-count closed)))
        (ok (loop for v being the hash-values of closed always (eq v t))
            "only ids are retained, not stream objects"))
      (ok (zerop (hash-table-count (http2-connection-streams conn))))
      (ok (= (http2-connection-last-stream-id conn) 1999))))

  (testing "frames for old and recent closed streams are still handled"
    (multiple-value-bind (conn err)
        (test-conn)
      (loop for id from 1 below 2000 by 2
            do (complete-stream conn id))
      (ok (stream-closed-p (connection-get-stream conn 1)))
      (connection-process-frame conn (make-window-update-frame 3 100))
      (ok (null (funcall err)) "WINDOW_UPDATE on a pruned closed stream is ignored")
      (connection-process-frame conn (make-rst-stream-frame 5 +protocol-error+))
      (ok (null (funcall err)) "RST_STREAM on a pruned closed stream is ignored")
      (connection-process-frame conn (make-data-frame 1 (ub8 4 1)))
      (ok (= (funcall err) +stream-closed+))
      (ok (equal (last-rst conn) (cons 1 +stream-closed+)))
      (connection-process-frame conn (make-data-frame 1997 (ub8 4 1)))
      (ok (equal (last-rst conn) (cons 1997 +stream-closed+)))
      (ok (not (http2-connection-goaway-sent conn)))
      (connection-process-frame
       conn (make-headers-frame 2001 (empty-octets) :end-headers t))
      (ok (stream-open-p (connection-get-stream conn 2001)))
      (ok (not (http2-connection-goaway-sent conn)))))

  (testing "an id above last-stream-id is still idle"
    (multiple-value-bind (conn err)
        (test-conn)
      (complete-stream conn 1)
      (connection-process-frame conn (make-window-update-frame 5 100))
      (ok (= (funcall err) +protocol-error+))
      (ok (http2-connection-goaway-sent conn))))

  (testing "HEADERS reusing a pruned closed id is a connection error"
    (multiple-value-bind (conn err)
        (test-conn)
      (loop for id from 1 below 2000 by 2
            do (complete-stream conn id))
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (ok (funcall err))
      (ok (http2-connection-goaway-sent conn)))))

(deftest connection-error-closes-connection
  (testing "a connection error sends GOAWAY, then closes"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((closes 0)
            (frames-at-close nil))
        (with-sent-frames (sent)
          (setf (woo.http2.connection::http2-connection-on-close conn)
                (lambda (c)
                  (declare (ignore c))
                  (incf closes)
                  (setf frames-at-close (sent))))
          (connection-process-frame conn (make-window-update-frame 0 0))
          (ok (= (funcall err) +protocol-error+))
          (ok (= closes 1))
          (ok (= (woo.http2.frames:frame-type (car (last frames-at-close)))
                 +frame-goaway+)
              "GOAWAY is queued before the close")
          (woo.http2.connection:connection-send-frame
           conn (make-ping-frame (ub8 8)))
          (ok (= (length (sent)) (length frames-at-close))
              "nothing is written after the close is queued")
          (woo.http2.connection::connection-protocol-error conn +protocol-error+)
          (ok (= closes 1) "close is idempotent")))))

  (testing "a stream error does not close the connection"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((closes 0))
        (setf (woo.http2.connection::http2-connection-on-close conn)
              (lambda (c) (declare (ignore c)) (incf closes)))
        (connection-process-frame
         conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
        (connection-process-frame conn (make-data-frame 1 (ub8 1 1)))
        (ok (= (funcall err) +stream-closed+))
        (ok (zerop closes)))))

  (testing "an HPACK error closes the connection"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((closes 0))
        (setf (woo.http2.connection::http2-connection-on-close conn)
              (lambda (c) (declare (ignore c)) (incf closes)))
        (connection-process-frame
         conn (make-headers-frame 1 (ub8 1 #xff) :end-headers t))
        (ok (= (funcall err) woo.http2.constants:+compression-error+))
        (ok (= closes 1))))))

(deftest local-settings-are-per-connection
  (testing "editing one connection's settings leaves new connections alone"
    (let* ((a (make-http2-connection))
           (settings (http2-connection-local-settings a)))
      (setf (cdr (assoc +settings-max-header-list-size+ settings)) 33)
      (let ((b (make-http2-connection)))
        (ok (not (eq settings (http2-connection-local-settings b))))
        (ok (= +default-max-header-list-size+
               (cdr (assoc +settings-max-header-list-size+
                           (http2-connection-local-settings b)))))
        (ok (= +default-max-header-list-size+
               (woo.http2.connection::connection-header-list-limit b)))))))

;;; Request body limits

(defun data-sink (conn)
  "Record each on-data call as (stream-id length end-stream), oldest first."
  (let ((calls nil))
    (setf (woo.http2.connection::http2-connection-on-data conn)
          (lambda (stream data end-stream)
            (push (list (http2-stream-id stream) (length data) end-stream) calls)))
    (lambda () (reverse calls))))

(defun count-rsts (frames &optional stream-id)
  (count-if (lambda (frame)
              (and (= (woo.http2.frames:frame-type frame) +frame-rst-stream+)
                   (or (null stream-id)
                       (= (woo.http2.frames:frame-stream-id frame) stream-id))))
            frames))

(deftest request-body-size-limit
  (testing "a body past *max-request-body-size* is RST CANCEL, the connection stays up"
    (let ((woo.http2.connection:*max-request-body-size* 1000))
      (multiple-value-bind (conn err)
          (test-conn)
        (let ((calls (data-sink conn)))
          (connection-process-frame
           conn (make-headers-frame 1 (empty-octets) :end-headers t))
          (connection-process-frame conn (make-data-frame 1 (ub8 600 1)))
          (ok (null (funcall err)))
          (connection-process-frame conn (make-data-frame 1 (ub8 600 1)))
          (ok (= (funcall err) +cancel+))
          (ok (equal (last-rst conn) (cons 1 +cancel+)))
          (ok (not (http2-connection-goaway-sent conn)))
          (ok (equal (funcall calls) '((1 600 nil)))
              "the over-limit frame is not buffered or delivered")
          (ok (stream-closed-p (connection-get-stream conn 1)))
          (ok (zerop (woo.http2.connection::http2-connection-buffered-body-octets conn))
              "the dropped stream releases its buffered octets")))))

  (testing "a body of exactly the limit is accepted"
    (let ((woo.http2.connection:*max-request-body-size* 1000))
      (multiple-value-bind (conn err)
          (test-conn)
        (connection-process-frame
         conn (make-headers-frame 1 (empty-octets) :end-headers t))
        (connection-process-frame conn (make-data-frame 1 (ub8 1000 1) :end-stream t))
        (ok (null (funcall err)))
        (ok (stream-half-closed-remote-p (connection-get-stream conn 1))))))

  (testing "a declared content-length above the limit is refused at HEADERS"
    (let ((woo.http2.connection:*max-request-body-size* 1000))
      (multiple-value-bind (conn err)
          (test-conn)
        (let ((headers-called nil))
          (setf (woo.http2.connection::http2-connection-on-headers conn)
                (lambda (stream headers end-stream)
                  (declare (ignore stream headers end-stream))
                  (setf headers-called t)))
          (connection-process-frame
           conn (headers-of 1 '(("content-length" . "1001"))))
          (ok (= (funcall err) +cancel+))
          (ok (equal (last-rst conn) (cons 1 +cancel+)))
          (ok (not headers-called))
          (ok (not (http2-connection-goaway-sent conn)))))))

  (testing "the per-connection body budget refuses the stream that would exceed it"
    (let ((woo.http2.connection:*max-request-body-size* nil)
          (woo.http2.connection:*max-connection-body-buffer* 1500))
      (multiple-value-bind (conn err)
          (test-conn)
        (dolist (id '(1 3 5))
          (connection-process-frame
           conn (make-headers-frame id (empty-octets) :end-headers t)))
        (connection-process-frame conn (make-data-frame 1 (ub8 1000 1)))
        (connection-process-frame conn (make-data-frame 3 (ub8 1000 1)))
        (ok (equal (last-rst conn) (cons 3 +cancel+)))
        (ok (stream-open-p (connection-get-stream conn 1)))
        (ok (= (woo.http2.connection::http2-connection-buffered-body-octets conn) 1000))
        ;; The client resets stream 1; its octets return to the budget.
        (connection-process-frame conn (make-rst-stream-frame 1 +cancel+))
        (ok (zerop (woo.http2.connection::http2-connection-buffered-body-octets conn)))
        (connection-process-frame conn (make-data-frame 5 (ub8 1000 1)))
        (ok (equal (last-rst conn) (cons 3 +cancel+)) "stream 5 fits again")
        (ok (stream-open-p (connection-get-stream conn 5)))
        (ok (= (funcall err) +cancel+))
        (ok (not (http2-connection-goaway-sent conn)))))))

(deftest request-body-buffer-grows-geometrically
  (testing "1000 appends reallocate the body buffer O(log n) times, not per frame"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((capacities nil))
        (setf (woo.http2.connection::http2-connection-on-data conn)
              (lambda (stream data end-stream)
                (declare (ignore data end-stream))
                (push (array-total-size
                       (woo.http2.stream:http2-stream-body-buffer stream))
                      capacities)))
        (connection-process-frame
         conn (make-headers-frame 1 (empty-octets) :end-headers t))
        (dotimes (i 1000)
          (connection-process-frame conn (make-data-frame 1 (ub8 100 (mod i 256)))))
        (ok (null (funcall err)))
        (let* ((capacities (nreverse capacities))
               (growths (loop for (a b) on capacities
                              count (and b (/= a b))))
               (body (woo.http2.stream:http2-stream-body-buffer
                      (connection-get-stream conn 1))))
          (ok (= (length capacities) 1000))
          ;; Doubling from 1024 to 100,000 octets is 7 growths. An exact-fit
          ;; buffer grows on every one of the 1000 frames.
          (ok (<= growths 10) (format nil "~D growths" growths))
          (ok (= (length body) 100000))
          (ok (<= (array-total-size body) (* 2 (length body))))
          (ok (loop for i below 1000
                    always (and (= (aref body (* i 100)) (mod i 256))
                                (= (aref body (+ (* i 100) 99)) (mod i 256))))
              "octets land in order"))))))

(deftest closed-stream-data-reset-limit
  (testing "DATA on closed streams earns RSTs only up to the limit, then GOAWAY"
    (let ((woo.http2.connection:*max-closed-stream-data-resets* 10))
      (multiple-value-bind (conn err)
          (test-conn)
        (with-sent-frames (sent)
          (connection-process-frame
           conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
          (dotimes (i 10)
            (connection-process-frame conn (make-data-frame 1 (ub8 100 1))))
          (ok (= (count-rsts (sent) 1) 10))
          (ok (= (funcall err) +stream-closed+))
          (ok (not (http2-connection-goaway-sent conn)))
          (connection-process-frame conn (make-data-frame 1 (ub8 100 1)))
          (ok (= (count-rsts (sent) 1) 10) "no RST past the limit")
          (ok (= (funcall err) +enhance-your-calm+))
          (ok (http2-connection-goaway-sent conn))
          (let ((goaway (find +frame-goaway+ (sent)
                              :key #'woo.http2.frames:frame-type)))
            (ok goaway)
            (ok (= (nth-value 1 (woo.http2.frames:parse-goaway-payload
                                 (frame-payload goaway)))
                   +enhance-your-calm+)))))))

  (testing "the default limit leaves ordinary stragglers alone"
    (ok (>= woo.http2.connection:*max-closed-stream-data-resets* 100))))

(deftest in-flight-data-after-our-rst-is-ignored
  (testing "DATA within the window the peer had at our RST is not RST again or counted"
    (let ((woo.http2.connection:*max-request-body-size* 1000))
      (multiple-value-bind (conn err)
          (test-conn)
        (with-sent-frames (sent)
          (connection-process-frame
           conn (make-headers-frame 1 (empty-octets) :end-headers t))
          (connection-process-frame conn (make-data-frame 1 (ub8 600 1)))
          (connection-process-frame conn (make-data-frame 1 (ub8 600 1)))
          (ok (= (funcall err) +cancel+))
          (ok (= (count-rsts (sent) 1) 1))
          ;; Both frames were debited, so the peer had 65535 - 1200 left.
          (let ((allowance (- +default-initial-window-size+ 1200)))
            (loop with left = allowance
                  while (plusp left)
                  do (let ((n (min left 16384)))
                       (connection-process-frame conn (make-data-frame 1 (ub8 n 1)))
                       (decf left n)))
            (ok (= (count-rsts (sent) 1) 1) "in-flight DATA gets no further RST")
            (ok (zerop (woo.http2.connection::http2-connection-closed-stream-data-resets conn))
                "in-flight DATA does not count toward the reset limit")
            (ok (not (http2-connection-goaway-sent conn)))
            (ok (> (http2-connection-window-size conn) (- +default-initial-window-size+ allowance))
                "ignored DATA still has its connection credit returned"))
          ;; Past the window the peer could have had, it is a violation again.
          (connection-process-frame conn (make-data-frame 1 (ub8 1 1)))
          (ok (= (count-rsts (sent) 1) 2))
          (ok (= (funcall err) +stream-closed+))
          (ok (= (woo.http2.connection::http2-connection-closed-stream-data-resets conn) 1))))))

  (testing "a stream that ended normally gets no in-flight allowance"
    (multiple-value-bind (conn err)
        (test-conn)
      (with-sent-frames (sent)
        (connection-process-frame
         conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
        (connection-process-frame conn (make-data-frame 1 (ub8 100 1)))
        (ok (= (count-rsts (sent) 1) 1))
        (ok (= (funcall err) +stream-closed+))
        ;; The first violation resets a half-closed (remote) stream: no allowance.
        (connection-process-frame conn (make-data-frame 1 (ub8 100 1)))
        (ok (= (count-rsts (sent) 1) 2))))))

;;; Trailer field validation

(defun send-trailers (trailers)
  "Open stream 1, send TRAILERS with END_STREAM. Returns (values conn err called)."
  (multiple-value-bind (conn err)
      (test-conn)
    (let ((calls 0))
      (setf (woo.http2.connection::http2-connection-on-headers conn)
            (lambda (stream headers end-stream)
              (declare (ignore stream headers end-stream))
              (incf calls)))
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (connection-process-frame conn (headers-of 1 trailers :end-stream t))
      (values conn err calls))))

(deftest trailer-field-validation
  (testing "malformed trailer fields are RST PROTOCOL_ERROR, not GOAWAY"
    (dolist (field `(("X-Upper" . "v")
                     ("connection" . "close")
                     ("keep-alive" . "5")
                     ("proxy-connection" . "keep-alive")
                     ("transfer-encoding" . "chunked")
                     ("upgrade" . "h2c")
                     ("te" . "gzip")
                     ("x-a" . ,(format nil "a~Cb" #\Return))
                     ("x-a" . ,(format nil "a~Cb" #\Newline))
                     ("x-a" . ,(format nil "a~Cb" #\Nul))
                     ("x-a" . " lead")
                     ("x-a" . "trail ")
                     ("x-a" . ,(format nil "trail~C" #\Tab))
                     ("bad name" . "v")
                     (":path" . "/")))
      (multiple-value-bind (conn err calls)
          (send-trailers (list field))
        (ok (= (funcall err) +protocol-error+) (car field))
        (ok (equal (last-rst conn) (cons 1 +protocol-error+)) (car field))
        (ok (= calls 1) "only the request headers reached on-headers")
        (ok (not (http2-connection-goaway-sent conn)) (car field)))))

  (testing "content-length in trailers is RST PROTOCOL_ERROR"
    (multiple-value-bind (conn err calls)
        (send-trailers '(("content-length" . "0")))
      (ok (= (funcall err) +protocol-error+))
      (ok (equal (last-rst conn) (cons 1 +protocol-error+)))
      (ok (= calls 1))
      (ok (not (http2-connection-goaway-sent conn)))))

  (testing "well-formed trailers, including TE: trailers, are accepted"
    (multiple-value-bind (conn err calls)
        (send-trailers '(("x-checksum" . "abc") ("te" . "trailers") ("x-empty" . "")))
      (ok (null (funcall err)))
      (ok (null (last-rst conn)))
      (ok (= calls 2))
      (ok (equal (woo.http2.stream:http2-stream-trailers (connection-get-stream conn 1))
                 '(("x-checksum" . "abc") ("te" . "trailers") ("x-empty" . "")))))))

(deftest max-frame-size-boundary
  (testing "a DATA frame of exactly 16384 octets is accepted at the default max"
    (multiple-value-bind (conn err)
        (test-conn)
      (let ((calls (data-sink conn)))
        (parse-bytes conn
                     (concat-octets
                      +connection-preface+
                      (serialize-frame (make-settings-frame nil))
                      (serialize-frame (make-headers-frame 1 (empty-octets) :end-headers t))
                      (serialize-frame (make-data-frame 1 (ub8 +default-max-frame-size+ 7)
                                                        :end-stream t))))
        (ok (= +default-max-frame-size+ 16384))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (equal (funcall calls) '((1 16384 t))))))))

;;; CONTINUATION flood (CVE-2024-27316 class)

(defun open-header-block (conn stream-id)
  "Start a header block on STREAM-ID that awaits CONTINUATION."
  (connection-process-frame
   conn (make-headers-frame stream-id (empty-octets) :end-headers nil)))

(deftest continuation-frame-count-is-capped
  (testing "empty CONTINUATIONs past the cap are GOAWAY ENHANCE_YOUR_CALM"
    (ok (<= woo.http2.connection:*max-continuation-frames* 64)
        "a literal: raising the default must fail this test")
    (multiple-value-bind (conn err)
        (test-conn)
      (with-sent-frames (sent)
        (open-header-block conn 1)
        (dotimes (i woo.http2.connection:*max-continuation-frames*)
          (connection-process-frame conn (make-continuation-frame 1 (empty-octets)
                                                                  :end-headers nil)))
        (ok (null (funcall err)) "up to the cap is accepted")
        (ok (= 1 (http2-connection-awaiting-continuation-stream-id conn)))
        (connection-process-frame conn (make-continuation-frame 1 (empty-octets)
                                                                :end-headers nil))
        (ok (= (funcall err) +enhance-your-calm+))
        (ok (http2-connection-goaway-sent conn))
        (ok (null (http2-connection-awaiting-continuation-stream-id conn)))
        (ok (null (woo.http2.connection::http2-connection-awaiting-continuation-stream conn)))
        (let ((goaway (find +frame-goaway+ (sent) :key #'woo.http2.frames:frame-type)))
          (ok goaway)
          (ok (= (nth-value 1 (woo.http2.frames:parse-goaway-payload
                               (frame-payload goaway)))
                 +enhance-your-calm+))))))

  (testing "100,000 empty CONTINUATIONs stop at the cap"
    (multiple-value-bind (conn err)
        (test-conn)
      (open-header-block conn 1)
      (let ((processed 0))
        (loop repeat 100000
              until (http2-connection-goaway-sent conn)
              do (connection-process-frame conn (make-continuation-frame 1 (empty-octets)
                                                                         :end-headers nil))
                 (incf processed))
        (ok (= processed (1+ woo.http2.connection:*max-continuation-frames*))
            (format nil "~D frames before GOAWAY" processed))
        (ok (= (funcall err) +enhance-your-calm+)))))

  (testing "a block split over the maximum number of CONTINUATIONs decodes"
    (multiple-value-bind (conn err)
        (test-conn)
      (let* ((headers '((":method" . "GET") (":scheme" . "https")
                        (":path" . "/split") (":authority" . "example.com")))
             (block (hpack-block headers))
             (n woo.http2.connection:*max-continuation-frames*)
             (got nil))
        (setf (woo.http2.connection::http2-connection-on-headers conn)
              (lambda (stream h end-stream)
                (declare (ignore stream end-stream))
                (setf got h)))
        (connection-process-frame
         conn (make-headers-frame 1 (subseq block 0 1) :end-headers nil :end-stream t))
        ;; The rest one octet at a time, padded out with empty frames.
        (loop for i from 1 below (length block)
              do (connection-process-frame
                  conn (make-continuation-frame 1 (subseq block i (1+ i)) :end-headers nil)))
        (loop repeat (- n (length block))
              do (connection-process-frame
                  conn (make-continuation-frame 1 (empty-octets) :end-headers nil)))
        (connection-process-frame conn (make-continuation-frame 1 (empty-octets)
                                                                :end-headers t))
        (ok (< (length block) n))
        (ok (null (funcall err)))
        (ok (not (http2-connection-goaway-sent conn)))
        (ok (equal got headers))
        (ok (stream-half-closed-remote-p (connection-get-stream conn 1)))))))

(deftest continuation-header-buffer-grows-geometrically
  (testing "a block over 4000 CONTINUATIONs copies O(n) octets, not O(n^2)"
    (let ((woo.http2.connection:*max-continuation-frames* 100000))
      (multiple-value-bind (conn err)
          (test-conn)
        (set-header-list-limit conn (* 1024 1024))
        (open-header-block conn 1)
        (let ((stream (connection-get-stream conn 1))
              (frames 4000)
              (chunk 16)
              (growths 0)
              (copied 0))
          (dotimes (i frames)
            (let* ((before (woo.http2.stream:http2-stream-header-buffer stream))
                   (capacity (if before (array-total-size before) 0))
                   (len (if before (length before) 0)))
              (connection-process-frame conn (make-continuation-frame 1 (ub8 chunk 1)
                                                                      :end-headers nil))
              ;; Each reallocation copies the octets already buffered.
              (let ((after (woo.http2.stream:http2-stream-header-buffer stream)))
                (when (/= (array-total-size after) capacity)
                  (incf growths)
                  (incf copied len)))))
          (ok (null (funcall err)))
          (ok (= (length (woo.http2.stream:http2-stream-header-buffer stream))
                 (* frames chunk)))
          ;; Doubling from 1024 to 64000 octets is 6 growths. An exact-fit
          ;; buffer grows on every frame and copies ~128 MB here.
          (ok (<= growths 10) (format nil "~D growths" growths))
          (ok (<= copied (* 2 frames chunk)) (format nil "~D octets copied" copied))
          (ok (<= (woo.http2.connection::http2-connection-header-octets-copied conn)
                  (* 4 frames chunk))
              "the connection's own count agrees"))))))

;;; HEADERS in flight when we reset a stream (RFC 9113 §5.1)

(defun reset-open-stream-1 (conn)
  "Open stream 1 and have us RST it CANCEL for its body size, as happens
   while the peer may still be sending trailers."
  (connection-process-frame conn (make-headers-frame 1 (empty-octets) :end-headers t))
  (connection-process-frame conn (make-data-frame 1 (ub8 5 1)))
  (ok (equal (last-rst conn) (cons 1 +cancel+))))

(defun check-dynamic-table-in-sync (conn)
  "Stream 3 references dynamic index 62, the entry custom-header-literal inserts."
  (let ((got nil))
    (setf (woo.http2.connection::http2-connection-on-headers conn)
          (lambda (stream headers end-stream)
            (declare (ignore stream end-stream))
            (setf got headers)))
    (connection-process-frame
     conn (make-headers-frame 3 (make-array 1 :element-type '(unsigned-byte 8)
                                              :initial-element #xbe)
                              :end-headers t :end-stream t))
    (ok (not (http2-connection-goaway-sent conn)))
    (ok (equal got '(("custom-key" . "custom-header")))
        "the ignored block was decoded into the dynamic table")))

(deftest trailers-in-flight-after-our-rst-are-ignored
  (testing "one HEADERS frame of trailers after our RST is decoded and ignored"
    (let ((woo.http2.connection:*max-request-body-size* 3))
      (multiple-value-bind (conn err)
          (test-conn)
        (with-sent-frames (sent)
          (reset-open-stream-1 conn)
          (connection-process-frame
           conn (make-headers-frame 1 (custom-header-literal)
                                    :end-headers t :end-stream t))
          (ok (= (funcall err) +cancel+) "no new error")
          (ok (not (http2-connection-goaway-sent conn)))
          (ok (= (count-rsts (sent) 1) 1) "no second RST")
          (ok (eq t (gethash 1 (woo.http2.connection::http2-connection-closed-streams conn)))
              "after END_STREAM no DATA allowance is left")
          (check-dynamic-table-in-sync conn)))))

  (testing "trailers split over HEADERS and CONTINUATION after our RST are ignored"
    (let ((woo.http2.connection:*max-request-body-size* 3))
      (multiple-value-bind (conn err)
          (test-conn)
        (with-sent-frames (sent)
          (reset-open-stream-1 conn)
          (let ((block (custom-header-literal)))
            (connection-process-frame
             conn (make-headers-frame 1 (subseq block 0 10)
                                      :end-headers nil :end-stream t))
            (ok (= 1 (http2-connection-awaiting-continuation-stream-id conn)))
            (connection-process-frame
             conn (make-continuation-frame 1 (subseq block 10 20) :end-headers nil))
            (connection-process-frame
             conn (make-continuation-frame 1 (subseq block 20) :end-headers t)))
          (ok (= (funcall err) +cancel+))
          (ok (not (http2-connection-goaway-sent conn)))
          (ok (null (http2-connection-awaiting-continuation-stream-id conn)))
          (ok (= (count-rsts (sent) 1) 1))
          (check-dynamic-table-in-sync conn)))))

  (testing "HEADERS on a stream that closed normally is still GOAWAY STREAM_CLOSED"
    (multiple-value-bind (conn err)
        (test-conn)
      (complete-stream conn 1)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers t :end-stream t))
      (ok (= (funcall err) +stream-closed+))
      (ok (http2-connection-goaway-sent conn))))

  (testing "split HEADERS on a stream that closed normally is GOAWAY STREAM_CLOSED"
    (multiple-value-bind (conn err)
        (test-conn)
      (complete-stream conn 1)
      (connection-process-frame
       conn (make-headers-frame 1 (empty-octets) :end-headers nil :end-stream t))
      (ok (null (funcall err)))
      (connection-process-frame
       conn (make-continuation-frame 1 (empty-octets) :end-headers t))
      (ok (= (funcall err) +stream-closed+))
      (ok (http2-connection-goaway-sent conn)))))

;;; Repeated SETTINGS

(deftest remote-settings-stay-bounded
  (testing "100 SETTINGS frames of 2730 entries keep one value per known id"
    (multiple-value-bind (conn err)
        (test-conn)
      (dotimes (k 100)
        (connection-process-frame
         conn (make-settings-frame
               (loop for i below 2730
                     collect (if (evenp i)
                                 (cons +settings-max-concurrent-streams+ (+ i k))
                                 ;; Unknown ids are ignored (RFC 9113 §6.5.2).
                                 (cons (+ #x100 (mod i 1000)) i))))))
      (ok (null (funcall err)))
      (ok (not (http2-connection-goaway-sent conn)))
      (let ((settings (http2-connection-remote-settings conn)))
        (ok (<= (length settings) 6) (format nil "~D entries" (length settings)))
        (ok (= (cdr (assoc +settings-max-concurrent-streams+ settings)) (+ 2728 99))
            "the latest value wins")
        (ok (null (assoc #x100 settings)) "unknown ids are not stored")))))

;;; WINDOW_UPDATE on closed streams (RFC 9113 §5.1, §6.9)

(deftest window-update-on-closed-stream-is-ignored
  (testing "the largest legal increment on a closed stream is not FLOW_CONTROL_ERROR"
    (multiple-value-bind (conn err)
        (test-conn)
      (complete-stream conn 1)
      (connection-process-frame conn (make-window-update-frame 1 +max-window-size+))
      (connection-process-frame conn (make-window-update-frame 1 +max-window-size+))
      (ok (null (funcall err)))
      (ok (not (http2-connection-goaway-sent conn)))))

  (testing "a stream we reset takes a large increment too"
    (let ((woo.http2.connection:*max-request-body-size* 3))
      (multiple-value-bind (conn err)
          (test-conn)
        (reset-open-stream-1 conn)
        (connection-process-frame conn (make-window-update-frame 1 +max-window-size+))
        (ok (= (funcall err) +cancel+))
        (ok (not (http2-connection-goaway-sent conn))))))

  (testing "increment 0 on a closed stream is ignored as well (§5.1)"
    (multiple-value-bind (conn err)
        (test-conn)
      (complete-stream conn 1)
      (with-sent-frames (sent)
        (connection-process-frame conn (make-window-update-frame 1 0))
        (ok (null (funcall err)))
        (ok (null (sent)) "no RST_STREAM or GOAWAY"))))

  (testing "an open stream still overflows at 2^31-1"
    (multiple-value-bind (conn err)
        (test-conn)
      (connection-process-frame conn (make-headers-frame 1 (empty-octets) :end-headers t))
      (connection-process-frame conn (make-window-update-frame 1 +max-window-size+))
      (ok (= (funcall err) +flow-control-error+)))))
