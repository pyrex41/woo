(in-package :cl-user)
(defpackage woo-test.websocket-e2e
  (:use :cl :rove)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-bytes-to-string)
  (:import-from :woo-test.showcase
                :showcase-loaded-p))
(in-package :woo-test.websocket-e2e)

;;; End-to-end WebSocket tests: a real woo server, driven over real sockets
;;; by a raw Lisp client (exact bytes, explicit fragments, invalid UTF-8)
;;; and by Node's built-in WebSocket client (what a browser does).

(defparameter *sample-key* "dGhlIHNhbXBsZSBub25jZQ==")
(defparameter *sample-accept* "s3pPLMBiTxaQ9kYGzzhZRbK+xOo=")

(defun crlf (&rest lines)
  (format nil "~{~A~C~C~}" (loop for l in lines append (list l #\Return #\Newline))))

(defun octets (&rest bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8) :initial-contents bytes))

(defun octets-string (octets)
  (map 'string #'code-char octets))

;;; The echo app. Every path upgrades; they differ in what the app returns
;;; afterwards and in how a close is answered.
;;;   /ws         returns NIL (woo would send a 500), default on-close
;;;   /ws-200     returns a finalized 200 the way a framework does
;;;   /ws-reason  on-close echoes the client's code and reason
;;; Text "close-me" makes the server close (4000 "bye"); "ping-me" makes it
;;; ping ("hi") and report the pong as text "pong:<payload>".

(defun echo-app (env)
  (let ((socket (getf env :clack.io))
        (path (getf env :path-info)))
    (if (not (woo:websocket-p env))
        '(200 (:content-type "text/plain") ("plain"))
        (let ((key (gethash "sec-websocket-key" (getf env :headers))))
          (woo:write-websocket-upgrade-response socket (woo:compute-accept-key key))
          (woo:setup-websocket
           socket
           :on-message (lambda (opcode payload)
                         (cond
                           ((= opcode woo.websocket:+opcode-binary+)
                            (woo:send-binary-frame socket payload))
                           (t
                            (let ((text (utf-8-bytes-to-string payload)))
                              (cond
                                ((string= text "close-me")
                                 (woo:send-close socket 4000 "bye"))
                                ((string= text "ping-me")
                                 (woo:send-ping socket (string-to-utf-8-bytes "hi")))
                                (t (woo:send-text-frame socket text)))))))
           :on-pong (lambda (payload)
                      (woo:send-text-frame socket (format nil "pong:~A"
                                                          (utf-8-bytes-to-string payload))))
           :on-close (and (string= path "/ws-reason")
                          (lambda (code reason)
                            (woo:send-close socket code reason))))
          (if (string= path "/ws-200")
              '(200 (:content-type "text/html") ("finalized by the framework"))
              nil)))))

(defmacro with-server ((port app) &body body)
  `(let ((clack.test:*clack-test-handler* :woo)
         (clack.test:*enable-debug* nil))
     (clack.test:testing-app "woo WebSocket server" ,app
       (let ((,port clack.test:*clack-test-port*))
         ,@body))))

;;; Raw client

(defstruct conn socket stream)

(defun conn-close (conn)
  (ignore-errors (usocket:socket-close (conn-socket conn))))

(defun read-with-timeout (thunk &optional (seconds 5))
  "THUNK's value, or :TIMEOUT."
  (handler-case (sb-ext:with-timeout seconds (funcall thunk))
    (sb-ext:timeout () :timeout)))

(defun read-exact (conn n)
  "N octets, or NIL on EOF or timeout."
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (let ((got (read-with-timeout
                (lambda () (read-sequence buf (conn-stream conn))))))
      (and (eql got n) buf))))

(defun read-until-eof (conn &optional (seconds 5))
  "Octets until the server closes (:TIMEOUT if it does not)."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (let ((r (read-with-timeout
              (lambda ()
                (handler-case
                    (loop for b = (read-byte (conn-stream conn) nil nil)
                          while b do (vector-push-extend b out))
                  (error () nil))
                out)
              seconds)))
      (if (eq r :timeout) :timeout (coerce out '(simple-array (unsigned-byte 8) (*)))))))

(defun available-octets (conn seconds)
  "Whatever arrives within SECONDS, without blocking past it."
  (sleep seconds)
  (let ((stream (conn-stream conn))
        (out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop while (listen stream)
          do (let ((b (read-byte stream nil nil)))
               (if b (vector-push-extend b out) (return))))
    out))

(defun send-octets (conn octets)
  (write-sequence octets (conn-stream conn))
  (force-output (conn-stream conn)))

(defun ws-open (port path &key (key *sample-key*))
  "Connect and send the upgrade request. Returns (values conn head), HEAD
   being the response up to and including the blank line, or NIL if none
   arrived within 5 seconds."
  (let* ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
         (conn (make-conn :socket socket :stream (usocket:socket-stream socket))))
    (send-octets conn (string-to-utf-8-bytes
                       (crlf (format nil "GET ~A HTTP/1.1" path)
                             (format nil "Host: 127.0.0.1:~D" port)
                             "Upgrade: websocket"
                             "Connection: Upgrade"
                             (format nil "Sec-WebSocket-Key: ~A" key)
                             "Sec-WebSocket-Version: 13"
                             "")))
    (let* ((end (octets 13 10 13 10))
           (head (read-with-timeout
                  (lambda ()
                    (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                                             :adjustable t :fill-pointer 0)))
                      (loop for b = (read-byte (conn-stream conn) nil nil)
                            while b
                            do (vector-push-extend b out)
                               (when (and (>= (length out) 4)
                                          (equalp (subseq out (- (length out) 4)) end))
                                 (return (octets-string out)))))))))
      (values conn (and (stringp head) head)))))

(defun client-frame (opcode payload &key (fin t))
  "A masked client frame."
  (let* ((len (length payload))
         (key (octets 55 250 33 61))
         (header (cond ((< len 126) (octets (logior (if fin #x80 0) opcode) (logior #x80 len)))
                       ((< len 65536) (octets (logior (if fin #x80 0) opcode) (logior #x80 126)
                                              (ldb (byte 8 8) len) (ldb (byte 8 0) len)))
                       (t (apply #'octets (logior (if fin #x80 0) opcode) (logior #x80 127)
                                 (loop for i from 7 downto 0 collect (ldb (byte 8 (* 8 i)) len))))))
         (frame (make-array (+ (length header) 4 len) :element-type '(unsigned-byte 8))))
    (replace frame header)
    (replace frame key :start1 (length header))
    (dotimes (i len)
      (setf (aref frame (+ (length header) 4 i))
            (logxor (aref payload i) (aref key (mod i 4)))))
    frame))

(defun close-body (code &optional (reason ""))
  (concatenate '(vector (unsigned-byte 8))
               (octets (ldb (byte 8 8) code) (ldb (byte 8 0) code))
               (string-to-utf-8-bytes reason)))

(defun read-frame (conn)
  "(values opcode payload fin) of the next server frame, or NIL on EOF or
   timeout. Server frames must not be masked."
  (let ((h (read-exact conn 2)))
    (when h
      (let* ((fin (logbitp 7 (aref h 0)))
             (opcode (logand (aref h 0) #x0F))
             (masked (logbitp 7 (aref h 1)))
             (len7 (logand (aref h 1) #x7F))
             (len (case len7
                    (126 (let ((e (read-exact conn 2)))
                           (and e (+ (ash (aref e 0) 8) (aref e 1)))))
                    (127 (let ((e (read-exact conn 8)))
                           (and e (reduce (lambda (a b) (+ (ash a 8) b)) e))))
                    (t len7))))
        (when (and len (not masked))
          (let ((payload (if (zerop len)
                             (make-array 0 :element-type '(unsigned-byte 8))
                             (read-exact conn len))))
            (when payload
              (values opcode payload fin))))))))

(defun close-code (payload)
  (and (>= (length payload) 2) (+ (ash (aref payload 0) 8) (aref payload 1))))

(defun close-reason (payload)
  (utf-8-bytes-to-string payload :start (min 2 (length payload))))

(defun expected-head (accept)
  (crlf "HTTP/1.1 101 Switching Protocols"
        "Upgrade: websocket"
        "Connection: Upgrade"
        (format nil "Sec-WebSocket-Accept: ~A" accept)
        ""))

;;; Raw-socket tests

(deftest e2e-handshake-and-nothing-after-it
  (with-server (port #'echo-app)
    (dolist (path '("/ws" "/ws-200"))
      (testing (format nil "~A: exact 101 bytes, then only frames" path)
        (multiple-value-bind (conn head) (ws-open port path)
          (unwind-protect
               (progn
                 (ok (equal head (expected-head *sample-accept*))
                     "the handshake is CR LF lines with the RFC 6455 accept key")
                 (ok (zerop (length (available-octets conn 0.3)))
                     "no HTTP response follows the 101")
                 (send-octets conn (client-frame 1 (string-to-utf-8-bytes "hello")))
                 (multiple-value-bind (opcode payload fin) (read-frame conn)
                   (ok (eql opcode 1) "a text frame comes back")
                   (ok fin)
                   (ok (equal (and payload (utf-8-bytes-to-string payload)) "hello")))
                 (ok (zerop (length (available-octets conn 0.2)))
                     "nothing else follows the echo"))
            (conn-close conn)))))))

(deftest e2e-fragments-ping-and-large-messages
  (with-server (port #'echo-app)
    (testing "explicit text fragments with a ping between them"
      (let ((conn (ws-open port "/ws")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 1 (string-to-utf-8-bytes "Hel") :fin nil))
               (send-octets conn (client-frame 9 (string-to-utf-8-bytes "p1")))
               (send-octets conn (client-frame 0 (string-to-utf-8-bytes "lo ") :fin nil))
               (send-octets conn (client-frame 0 (string-to-utf-8-bytes "world")))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 10) "the interleaved ping is answered first")
                 (ok (equalp payload (string-to-utf-8-bytes "p1"))))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 1))
                 (ok (equal (and payload (utf-8-bytes-to-string payload)) "Hello world")
                     "the fragments are reassembled")))
          (conn-close conn))))
    (testing "a 1.5 MB binary message sent as 24 fragments"
      (let* ((conn (ws-open port "/ws"))
             (n (* 24 65536))
             (message (make-array n :element-type '(unsigned-byte 8))))
        (dotimes (i n) (setf (aref message i) (mod (* i 7) 256)))
        (unwind-protect
             (progn
               (loop for start from 0 below n by 65536
                     for first = t then nil
                     do (send-octets conn (client-frame (if first 2 0)
                                                        (subseq message start (+ start 65536))
                                                        :fin (= (+ start 65536) n))))
               (multiple-value-bind (opcode payload fin) (read-frame conn)
                 (ok (eql opcode 2) "echoed as binary")
                 (ok fin)
                 (ok (equalp payload message) "every octet comes back")))
          (conn-close conn))))
    (testing "client ping gets a pong with the same payload"
      (let ((conn (ws-open port "/ws")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 9 (octets 1 2 3)))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 10))
                 (ok (equalp payload (octets 1 2 3)))))
          (conn-close conn))))
    (testing "server ping, client pong"
      (let ((conn (ws-open port "/ws")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 1 (string-to-utf-8-bytes "ping-me")))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 9))
                 (ok (equalp payload (string-to-utf-8-bytes "hi")))
                 (send-octets conn (client-frame 10 payload)))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 1))
                 (ok (equal (and payload (utf-8-bytes-to-string payload)) "pong:hi"))))
          (conn-close conn))))))

(deftest e2e-closing-handshake
  (with-server (port #'echo-app)
    (testing "client close: the default handler answers with the code, then closes"
      (let ((conn (ws-open port "/ws")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 8 (close-body 4001 "done")))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 8) "a close frame comes back")
                 (ok (eql (close-code payload) 4001)))
               (ok (equalp (read-until-eof conn) #()) "then the server closes the socket"))
          (conn-close conn))))
    (testing "client close: an app echoing the reason"
      (let ((conn (ws-open port "/ws-reason")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 8 (close-body 4002 "see you")))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 8))
                 (ok (eql (close-code payload) 4002))
                 (ok (equal (close-reason payload) "see you")))
               (ok (equalp (read-until-eof conn) #())))
          (conn-close conn))))
    (testing "server close: the server waits for the client's close, then closes"
      (let ((conn (ws-open port "/ws")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 1 (string-to-utf-8-bytes "close-me")))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 8))
                 (ok (eql (close-code payload) 4000))
                 (ok (equal (close-reason payload) "bye")))
               (ok (zerop (length (available-octets conn 0.2)))
                   "the socket stays open for the client's close")
               (send-octets conn (client-frame 8 (close-body 4000)))
               (ok (equalp (read-until-eof conn) #()) "no second close; the socket closes"))
          (conn-close conn))))
    (testing "invalid UTF-8 in a text message fails the connection with 1007"
      (let ((conn (ws-open port "/ws")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 1 (octets #xFF #xFE #x41)))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 8))
                 (ok (eql (close-code payload) 1007)))
               (ok (equalp (read-until-eof conn) #())))
          (conn-close conn))))))

;;; Node's WebSocket client (global in Node 22), the same implementation
;;; family a browser uses: it rejects a malformed handshake and fails the
;;; connection on garbage after the 101.

(defparameter *node-client* "
const [base, mode] = process.argv.slice(2);
const out = (ok, name, detail) =>
  console.log((ok ? 'PASS ' : 'FAIL ') + name + (detail !== undefined ? ' ' + detail : ''));
function open(url) {
  return new Promise((resolve, reject) => {
    const ws = new WebSocket(url);
    ws.binaryType = 'arraybuffer';
    ws.onopen = () => resolve(ws);
    ws.onerror = () => reject(new Error('could not open ' + url));
    ws.onclose = (e) => reject(new Error('closed before open ' + e.code));
  });
}
function next(ws) {
  return new Promise((resolve, reject) => {
    ws.onmessage = (e) => resolve(e.data);
    ws.onclose = (e) => reject(new Error('closed ' + e.code));
  });
}
function closed(ws) {
  return new Promise((resolve) => { ws.onmessage = null; ws.onclose = resolve; });
}
const same = (a, b) => Buffer.from(a).equals(Buffer.from(b));
async function core() {
  let ws = await open(base + '/ws');
  out(true, 'handshake');
  ws.send('hello'); let m = await next(ws);
  out(m === 'hello', 'text-echo', JSON.stringify(m));
  const bin = new Uint8Array([0, 1, 2, 255, 254, 128]);
  ws.send(bin); m = await next(ws);
  out(m instanceof ArrayBuffer && same(m, bin), 'binary-echo');
  const big = 'abc\\u00e9\\u20ac'.repeat(300000);
  ws.send(big); m = await next(ws);
  out(m === big, 'large-echo', typeof m === 'string' ? m.length : typeof m);
  ws.send('ping-me'); m = await next(ws);
  out(m === 'pong:hi', 'server-ping-pong', JSON.stringify(m));
  let c = closed(ws); ws.close(4001, 'done'); let e = await c;
  out(e.code === 4001 && e.wasClean, 'client-close', e.code + ' ' + e.wasClean);
  ws = await open(base + '/ws-reason');
  c = closed(ws); ws.close(4002, 'see you'); e = await c;
  out(e.code === 4002 && e.reason === 'see you' && e.wasClean, 'client-close-reason',
      e.code + ' ' + JSON.stringify(e.reason) + ' ' + e.wasClean);
  ws = await open(base + '/ws');
  c = closed(ws); ws.send('close-me'); e = await c;
  out(e.code === 4000 && e.reason === 'bye' && e.wasClean, 'server-close',
      e.code + ' ' + JSON.stringify(e.reason) + ' ' + e.wasClean);
}
async function showcase() {
  const http = base.replace(/^ws/, 'http');
  const res = await fetch(http + '/api/benchmarks');
  const data = await res.json();
  const types = { name: 'string', language: 'string', requestsPerSec: 'number',
                  latencyAvgMs: 'number', latencyMaxMs: 'number', highlight: 'boolean' };
  const bad = data.flatMap((d) => Object.entries(types)
    .filter(([k, t]) => typeof d[k] !== t).map(([k]) => d.name + '.' + k));
  out(Array.isArray(data) && data.length > 0 && bad.length === 0, 'benchmarks-json', bad.join(','));
  let ws = await open(base + '/ws/echo');
  out(true, 'showcase-handshake');
  ws.send('hi there'); let m = await next(ws);
  out(m === 'hi there', 'showcase-text-echo', JSON.stringify(m));
  const bin = new Uint8Array([0xff, 0xfe, 0x00, 0x80]);
  ws.send(bin); m = await next(ws);
  out(m instanceof ArrayBuffer && same(m, bin), 'showcase-binary-echo');
  const c = closed(ws); ws.close(4003, 'bye'); const e = await c;
  out(e.code === 4003 && e.wasClean, 'showcase-client-close', e.code + ' ' + e.wasClean);
}
setTimeout(() => { out(false, 'timeout'); process.exit(2); }, 20000);
(mode === 'showcase' ? showcase() : core()).then(
  () => process.exit(0),
  (err) => { out(false, 'exception', err.message); process.exit(1); });
")

(defun node-available-p ()
  (ignore-errors
   (multiple-value-bind (out err code)
       (uiop:run-program '("node" "-e" "process.stdout.write(typeof WebSocket)")
                         :output :string :error-output nil :ignore-error-status t)
     (declare (ignore err))
     (and (eql code 0) (string= out "function")))))

(defun run-node-client (port mode &key (timeout 30))
  "Run the Node client against PORT. Returns its output lines, or
   (\"FAIL node-timeout\") if it outlives TIMEOUT seconds."
  (let ((script (uiop:with-temporary-file (:stream s :pathname p :keep t :type "js")
                  (write-string *node-client* s)
                  p)))
    (unwind-protect
         (let ((proc (uiop:launch-program
                      (list "node" (uiop:native-namestring script)
                            (format nil "ws://127.0.0.1:~D" port) mode)
                      :output :stream :error-output :output)))
           (loop repeat (* timeout 10)
                 while (uiop:process-alive-p proc)
                 do (sleep 0.1))
           (if (uiop:process-alive-p proc)
               (progn (uiop:terminate-process proc :urgent t)
                      (uiop:wait-process proc)
                      (list "FAIL node-timeout"))
               (progn
                 (uiop:wait-process proc)
                 (uiop:slurp-stream-lines (uiop:process-info-output proc)))))
      (ignore-errors (delete-file script)))))

(defun check-node-results (lines expected)
  (dolist (name expected)
    (let ((line (find-if (lambda (l)
                           (let ((p (search name l)))
                             (and p (= p 5)
                                  (or (= (length l) (+ 5 (length name)))
                                      (char= (char l (+ 5 (length name))) #\Space)))))
                         lines)))
      (ok (and line (eql 0 (search "PASS " line)))
          (format nil "node: ~A~@[ (~A)~]" name (or line "no result")))))
  (let ((failures (remove-if-not (lambda (l) (eql 0 (search "FAIL " l))) lines)))
    (ok (null failures) (format nil "node reported no failures~{; ~A~}" failures))))

(deftest e2e-node-client
  (if (not (node-available-p))
      (skip "node with a global WebSocket is not on PATH; Node WebSocket e2e tests skipped")
      (with-server (port #'echo-app)
        (check-node-results (run-node-client port "core")
                            '("handshake" "text-echo" "binary-echo" "large-echo"
                              "server-ping-pong" "client-close" "client-close-reason"
                              "server-close")))))

;;; The showcase app: /ws/echo and /api/benchmarks over a real server.

(defun showcase-handler ()
  ;; Drop the access log middleware's output from the test log.
  (let ((handler (symbol-value (find-symbol "*HANDLER*" :woo-showcase))))
    (lambda (env)
      (let ((*standard-output* (make-broadcast-stream)))
        (funcall handler env)))))

(defmacro with-showcase ((port) &body body)
  `(if (not (showcase-loaded-p))
       (skip "woo-showcase is not loadable (missing dependency); run (ql:quickload :woo-showcase) once. Showcase e2e tests skipped")
       (with-server (,port (showcase-handler))
         ,@body)))

(defun http-get (port path)
  "Open a connection and send GET PATH with Connection: close."
  (let* ((socket (usocket:socket-connect "127.0.0.1" port :element-type '(unsigned-byte 8)))
         (conn (make-conn :socket socket :stream (usocket:socket-stream socket))))
    (send-octets conn (string-to-utf-8-bytes
                       (crlf (format nil "GET ~A HTTP/1.1" path)
                             "Host: 127.0.0.1"
                             "Connection: close"
                             "")))
    conn))

(deftest e2e-showcase-websocket
  (with-showcase (port)
    (testing "/ws/echo upgrades with nothing after the 101"
      (multiple-value-bind (conn head) (ws-open port "/ws/echo")
        (unwind-protect
             (progn
               (ok (equal head (expected-head *sample-accept*)))
               (ok (zerop (length (available-octets conn 0.3)))
                   "ningle's 200 is not written into the WebSocket stream"))
          (conn-close conn))))
    (testing "binary is echoed as binary, even when it is not UTF-8"
      (let ((conn (ws-open port "/ws/echo")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 2 (octets #xFF #xFE #x00 #x80)))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 2))
                 (ok (equalp payload (octets #xFF #xFE #x00 #x80))))
               (send-octets conn (client-frame 1 (string-to-utf-8-bytes (format nil "caf~C" (code-char #xE9)))))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 1) "text stays text")
                 (ok (equalp payload (string-to-utf-8-bytes (format nil "caf~C" (code-char #xE9)))))))
          (conn-close conn))))
    (testing "the server answers the client's close"
      (let ((conn (ws-open port "/ws/echo")))
        (unwind-protect
             (progn
               (send-octets conn (client-frame 8 (close-body 1001 "leaving")))
               (multiple-value-bind (opcode payload) (read-frame conn)
                 (ok (eql opcode 8) "a close frame comes back")
                 (ok (eql (and payload (close-code payload)) 1001)))
               (ok (equalp (read-until-eof conn 3) #()) "and the socket is closed"))
          (conn-close conn))))))

(deftest e2e-showcase-slow-resource-does-not-block
  (with-showcase (port)
    (testing "a 1 s /api/resource request does not stall a concurrent request"
      (let* ((start (get-internal-real-time))
             (slow (http-get port "/api/resource/7?delay=1000")))
        (unwind-protect
             (progn
               (sleep 0.1)                ; let the server start the slow one
               (let* ((fast (http-get port "/api/benchmarks"))
                      (fast-res (unwind-protect (read-until-eof fast 5)
                                  (conn-close fast)))
                      (fast-elapsed (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second)))
                 (ok (and (vectorp fast-res)
                          (search "HTTP/1.1 200" (octets-string fast-res)))
                     "the fast request succeeds")
                 (ok (< fast-elapsed 0.7)
                     (format nil "the fast request finished after ~,2Fs, before the slow one"
                             fast-elapsed)))
               (let* ((slow-res (read-until-eof slow 5))
                      (slow-elapsed (/ (- (get-internal-real-time) start)
                                       internal-time-units-per-second))
                      (text (if (vectorp slow-res) (octets-string slow-res) "")))
                 (ok (search "HTTP/1.1 200" text) "the slow request succeeds")
                 (ok (search "\"id\":\"7\"" text))
                 (ok (search "\"delay\":1000" text))
                 (ok (>= slow-elapsed 0.95)
                     (format nil "the slow request still waited (~,2Fs)" slow-elapsed))))
          (conn-close slow))))))

(deftest e2e-showcase-node-client
  (cond
    ((not (node-available-p))
     (skip "node with a global WebSocket is not on PATH; showcase Node e2e tests skipped"))
    (t
     (with-showcase (port)
       (check-node-results (run-node-client port "showcase")
                           '("benchmarks-json" "showcase-handshake" "showcase-text-echo"
                             "showcase-binary-echo" "showcase-client-close"))))))
