(in-package :cl-user)
(defpackage woo.http2.hpack
  (:use :cl)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes
                :utf-8-bytes-to-string)
  (:export :make-hpack-context
           :hpack-context
           :hpack-context-max-dynamic-table-size
           :hpack-encode-headers
           :hpack-decode-headers
           :hpack-context-update-size
           :hpack-compression-error))
(in-package :woo.http2.hpack)

(define-condition hpack-compression-error (error)
  ((reason :initarg :reason :reader hpack-compression-error-reason))
  (:report (lambda (c stream)
             (format stream "HPACK compression error: ~A"
                     (hpack-compression-error-reason c)))))

(defun hpack-error (reason)
  (error 'hpack-compression-error :reason reason))

;; Static table (RFC 7541 Appendix A)
;; Index 1-61, stored as (name . value) pairs
(defvar *static-table*
  #(nil  ; Index 0 is invalid
    (":authority" . "")
    (":method" . "GET")
    (":method" . "POST")
    (":path" . "/")
    (":path" . "/index.html")
    (":scheme" . "http")
    (":scheme" . "https")
    (":status" . "200")
    (":status" . "204")
    (":status" . "206")
    (":status" . "304")
    (":status" . "400")
    (":status" . "404")
    (":status" . "500")
    ("accept-charset" . "")
    ("accept-encoding" . "gzip, deflate")
    ("accept-language" . "")
    ("accept-ranges" . "")
    ("accept" . "")
    ("access-control-allow-origin" . "")
    ("age" . "")
    ("allow" . "")
    ("authorization" . "")
    ("cache-control" . "")
    ("content-disposition" . "")
    ("content-encoding" . "")
    ("content-language" . "")
    ("content-length" . "")
    ("content-location" . "")
    ("content-range" . "")
    ("content-type" . "")
    ("cookie" . "")
    ("date" . "")
    ("etag" . "")
    ("expect" . "")
    ("expires" . "")
    ("from" . "")
    ("host" . "")
    ("if-match" . "")
    ("if-modified-since" . "")
    ("if-none-match" . "")
    ("if-range" . "")
    ("if-unmodified-since" . "")
    ("last-modified" . "")
    ("link" . "")
    ("location" . "")
    ("max-forwards" . "")
    ("proxy-authenticate" . "")
    ("proxy-authorization" . "")
    ("range" . "")
    ("referer" . "")
    ("refresh" . "")
    ("retry-after" . "")
    ("server" . "")
    ("set-cookie" . "")
    ("strict-transport-security" . "")
    ("transfer-encoding" . "")
    ("user-agent" . "")
    ("vary" . "")
    ("via" . "")
    ("www-authenticate" . "")))

(defstruct hpack-context
  "HPACK encoder/decoder context with dynamic table."
  (dynamic-table (make-array 0 :adjustable t :fill-pointer 0))
  (dynamic-table-size 0 :type fixnum)
  (max-dynamic-table-size 4096 :type fixnum))

(defun hpack-entry-size (name value)
  "Calculate entry size per RFC 7541 Section 4.1.
   Size = length of name + length of value + 32 bytes overhead."
  (+ 32
     (length name)
     (length value)))

(defun hpack-context-evict (ctx)
  "Evict entries from dynamic table until size <= max size."
  (loop while (and (> (length (hpack-context-dynamic-table ctx)) 0)
                   (> (hpack-context-dynamic-table-size ctx)
                      (hpack-context-max-dynamic-table-size ctx)))
        do (let* ((table (hpack-context-dynamic-table ctx))
                  (entry (aref table (1- (length table)))))
             ;; Remove oldest entry (at the end)
             (decf (hpack-context-dynamic-table-size ctx)
                   (hpack-entry-size (car entry) (cdr entry)))
             (vector-pop table))))

(defun hpack-context-add-entry (ctx name value)
  "Add entry to dynamic table (at the beginning, index 62)."
  (let ((size (hpack-entry-size name value)))
    (when (<= size (hpack-context-max-dynamic-table-size ctx))
      ;; Add to front by shifting all elements
      (let ((table (hpack-context-dynamic-table ctx)))
        (vector-push-extend nil table)
        ;; Shift elements right
        (loop for i from (1- (length table)) downto 1
              do (setf (aref table i) (aref table (1- i))))
        ;; Insert new entry at front
        (setf (aref table 0) (cons name value)))
      (incf (hpack-context-dynamic-table-size ctx) size)
      (hpack-context-evict ctx))))

(defun hpack-context-update-size (ctx new-size)
  "Update max dynamic table size and evict if necessary."
  (setf (hpack-context-max-dynamic-table-size ctx) new-size)
  (hpack-context-evict ctx))

(defun hpack-lookup-index (ctx index)
  "Look up entry by index (1-based).
   Indices 1-61 are static table, 62+ are dynamic table."
  (let ((static-len (1- (length *static-table*))))  ; 61 entries
    (cond
      ((zerop index)
       (hpack-error "invalid index 0"))
      ((<= index static-len)
       (aref *static-table* index))
      (t
       (let ((dyn-idx (- index static-len 1))
             (dyn-table (hpack-context-dynamic-table ctx)))
         (unless (and (>= dyn-idx 0) (< dyn-idx (length dyn-table)))
           (hpack-error (format nil "unknown index ~A" index)))
         (aref dyn-table dyn-idx))))))

(defun hpack-find-header (ctx name value)
  "Find header in tables. Returns (values index name-only-p).
   INDEX is the table index if found, NAME-ONLY-P is T if only name matches."
  ;; Search static table first
  (loop for i from 1 below (length *static-table*)
        for entry = (aref *static-table* i)
        when (string= (car entry) name)
        do (if (string= (cdr entry) value)
               (return-from hpack-find-header (values i nil))
               (return-from hpack-find-header (values i t))))
  ;; Search dynamic table
  (loop for i from 0 below (length (hpack-context-dynamic-table ctx))
        for entry = (aref (hpack-context-dynamic-table ctx) i)
        when (string= (car entry) name)
        do (let ((idx (+ 62 i)))
             (if (string= (cdr entry) value)
                 (return-from hpack-find-header (values idx nil))
                 (return-from hpack-find-header (values idx t)))))
  (values nil nil))

;;; Integer encoding/decoding (RFC 7541 Section 5.1)

;; RFC 7541 §5.1: bound extra octets so a 0x80 chain cannot run past the
;; remaining buffer or grow without limit. Five continuation bytes yield
;; a 35-bit addend, which covers any legitimate header-table index/size.
(defconstant +hpack-integer-max-extra-octets+ 5)
(defconstant +hpack-integer-max-value+ #xFFFFFFFF)

(defun hpack-decode-integer (data start prefix-bits &optional (end (length data)))
  "Decode HPACK integer starting at START with PREFIX-BITS prefix.
   Returns (values integer bytes-consumed). Signals COMPRESSION_ERROR
   if the encoding overruns END, uses too many octets, or overflows."
  (unless (< start end)
    (hpack-error "integer truncated"))
  (let* ((max-prefix (1- (ash 1 prefix-bits)))
         (value (logand (aref data start) max-prefix))
         (idx 1))
    (when (= value max-prefix)
      (loop for extra from 0
            do (when (>= extra +hpack-integer-max-extra-octets+)
                 (hpack-error "integer too long"))
               (when (>= (+ start idx) end)
                 (hpack-error "integer truncated"))
               (let ((byte (aref data (+ start idx))))
                 (incf value (ash (logand byte #x7F) (* extra 7)))
                 (incf idx)
                 (when (> value +hpack-integer-max-value+)
                   (hpack-error "integer overflow"))
                 (unless (logbitp 7 byte)
                   (return)))))
    (values value idx)))

(defun hpack-encode-integer (value prefix-bits first-byte)
  "Encode HPACK integer with PREFIX-BITS prefix.
   FIRST-BYTE contains the non-prefix bits in the first byte.
   Returns list of bytes."
  (let ((max-prefix (1- (ash 1 prefix-bits))))
    (if (< value max-prefix)
        (list (logior first-byte value))
        (let ((bytes (list (logior first-byte max-prefix))))
          (decf value max-prefix)
          (loop while (>= value 128)
                do (push (logior #x80 (logand value #x7F)) bytes)
                   (setf value (ash value -7)))
          (push value bytes)
          (nreverse bytes)))))

;;; Huffman coding (RFC 7541 Appendix B)

(defparameter *huffman-codes*
  #(#x1ff8 #x7fffd8 #xfffffe2 #xfffffe3 #xfffffe4 #xfffffe5 #xfffffe6 #xfffffe7
    #xfffffe8 #xffffea #x3ffffffc #xfffffe9 #xfffffea #x3ffffffd #xfffffeb #xfffffec
    #xfffffed #xfffffee #xfffffef #xffffff0 #xffffff1 #xffffff2 #x3ffffffe #xffffff3
    #xffffff4 #xffffff5 #xffffff6 #xffffff7 #xffffff8 #xffffff9 #xffffffa #xffffffb
    #x14 #x3f8 #x3f9 #xffa #x1ff9 #x15 #xf8 #x7fa #x3fa #x3fb #xf9 #x7fb #xfa
    #x16 #x17 #x18 #x0 #x1 #x2 #x19 #x1a #x1b #x1c #x1d #x1e #x1f #x5c #xfb
    #x7ffc #x20 #xffb #x3fc #x1ffa #x21 #x5d #x5e #x5f #x60 #x61 #x62 #x63
    #x64 #x65 #x66 #x67 #x68 #x69 #x6a #x6b #x6c #x6d #x6e #x6f #x70 #x71
    #x72 #xfc #x73 #xfd #x1ffb #x7fff0 #x1ffc #x3ffc #x22 #x7ffd #x3 #x23
    #x4 #x24 #x5 #x25 #x26 #x27 #x6 #x74 #x75 #x28 #x29 #x2a #x7 #x2b
    #x76 #x2c #x8 #x9 #x2d #x77 #x78 #x79 #x7a #x7b #x7ffe #x7fc #x3ffd #x1ffd
    #xffffffc #xfffe6 #x3fffd2 #xfffe7 #xfffe8 #x3fffd3 #x3fffd4 #x3fffd5
    #x7fffd9 #x3fffd6 #x7fffda #x7fffdb #x7fffdc #x7fffdd #x7fffde #xffffeb
    #x7fffdf #xffffec #xffffed #x3fffd7 #x7fffe0 #xffffee #x7fffe1 #x7fffe2
    #x7fffe3 #x7fffe4 #x1fffdc #x3fffd8 #x7fffe5 #x3fffd9 #x7fffe6 #x7fffe7
    #xffffef #x3fffda #x1fffdd #xfffe9 #x3fffdb #x3fffdc #x7fffe8 #x7fffe9
    #x1fffde #x7fffea #x3fffdd #x3fffde #xfffff0 #x1fffdf #x3fffdf #x7fffeb
    #x7fffec #x1fffe0 #x1fffe1 #x3fffe0 #x1fffe2 #x7fffed #x3fffe1 #x7fffee
    #x7fffef #xfffea #x3fffe2 #x3fffe3 #x3fffe4 #x7ffff0 #x3fffe5 #x3fffe6
    #x7ffff1 #x3ffffe0 #x3ffffe1 #xfffeb #x7fff1 #x3fffe7 #x7ffff2 #x3fffe8
    #x1ffffec #x3ffffe2 #x3ffffe3 #x3ffffe4 #x7ffffde #x7ffffdf #x3ffffe5
    #xfffff1 #x1ffffed #x7fff2 #x1fffe3 #x3ffffe6 #x7ffffe0 #x7ffffe1
    #x3ffffe7 #x7ffffe2 #xfffff2 #x1fffe4 #x1fffe5 #x3ffffe8 #x3ffffe9
    #xffffffd #x7ffffe3 #x7ffffe4 #x7ffffe5 #xfffec #xfffff3 #xfffed
    #x1fffe6 #x3fffe9 #x1fffe7 #x1fffe8 #x7ffff3 #x3fffea #x3fffeb
    #x1ffffee #x1ffffef #xfffff4 #xfffff5 #x3ffffea #x7ffff4 #x3ffffeb
    #x7ffffe6 #x3ffffec #x3ffffed #x7ffffe7 #x7ffffe8 #x7ffffe9 #x7ffffea
    #x7ffffeb #xffffffe #x7ffffec #x7ffffed #x7ffffee #x7ffffef #x7fffff0
    #x3ffffee))

(defparameter *huffman-code-len*
  #(13 23 28 28 28 28 28 28 28 24 30 28 28 30 28 28
    28 28 28 28 28 28 30 28 28 28 28 28 28 28 28 28
    6 10 10 12 13 6 8 11 10 10 8 11 8 6 6 6
    5 5 5 6 6 6 6 6 6 6 7 8 15 6 12 10
    13 6 7 7 7 7 7 7 7 7 7 7 7 7 7 7
    7 7 7 7 7 7 7 7 8 7 8 13 19 13 14 6
    15 5 6 5 6 5 6 6 6 5 7 7 6 6 6 5
    6 7 6 5 5 6 7 7 7 7 7 15 11 14 13 28
    20 22 20 20 22 22 22 23 22 23 23 23 23 23 24 23
    24 24 22 23 24 23 23 23 23 21 22 23 22 23 23 24
    22 21 20 22 22 23 23 21 23 22 22 24 21 22 23 23
    21 21 22 21 23 22 23 23 20 22 22 22 23 22 22 23
    26 26 20 19 22 23 22 25 26 26 26 27 27 26 24 25
    19 21 26 27 27 26 27 24 21 21 26 26 28 27 27 27
    20 24 20 21 22 21 21 23 22 22 25 25 24 24 26 23
    26 27 26 26 27 27 27 27 27 28 27 27 27 27 27 26))

(defun huffman-insert-code (node code nbits symbol)
  (loop for i from (1- nbits) downto 0
        for bit = (ldb (byte 1 i) code)
        do (unless (consp node)
             (hpack-error "invalid Huffman table"))
           (let ((child (if (zerop bit) (car node) (cdr node))))
             (when (and (zerop i) child)
               (hpack-error "invalid Huffman table"))
             (unless child
               (setf child (if (zerop i) symbol (cons nil nil)))
               (if (zerop bit)
                   (setf (car node) child)
                   (setf (cdr node) child)))
             (setf node child)))
  node)

(defun make-huffman-tree ()
  (let ((root (cons nil nil)))
    (loop for sym from 0 below 256
          do (huffman-insert-code root
                                  (aref *huffman-codes* sym)
                                  (aref *huffman-code-len* sym)
                                  sym))
    root))

(defparameter *huffman-tree* (make-huffman-tree))

(defun huffman-decode-bytes (data start end)
  "Decode Huffman-coded octets in DATA[START,END). Padding must be a
   prefix of the EOS all-ones pattern (RFC 7541 §5.2)."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0))
        (node *huffman-tree*)
        (accept-eos t)
        (bits-left 0))
    (loop for i from start below end
          for byte = (aref data i)
          do (loop for b from 7 downto 0
                   do (setf accept-eos nil)
                      (incf bits-left)
                      (when (> bits-left 30)
                        (hpack-error "Huffman code too long"))
                      (let ((bit (ldb (byte 1 b) byte)))
                        (setf node (if (zerop bit) (car node) (cdr node)))
                        (cond
                          ((null node)
                           (hpack-error "invalid Huffman sequence"))
                          ((integerp node)
                           (vector-push-extend node out)
                           (setf node *huffman-tree*
                                 accept-eos t
                                 bits-left 0))))))
    (unless accept-eos
      (unless (<= bits-left 7)
        (hpack-error "invalid Huffman padding"))
      (let* ((last (aref data (1- end)))
             (mask (1- (ash 1 bits-left))))
        (unless (= (logand last mask) mask)
          (hpack-error "invalid Huffman padding"))))
    out))

(defun huffman-encode-bytes (bytes)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8)
                         :adjustable t :fill-pointer 0))
        (acc 0)
        (nbits 0))
    (loop for b across bytes
          for code = (aref *huffman-codes* b)
          for len = (aref *huffman-code-len* b)
          do (setf acc (logior (ash acc len) code))
             (incf nbits len)
             (loop while (>= nbits 8)
                   do (decf nbits 8)
                      (vector-push-extend (ldb (byte 8 nbits) acc) out)
                      (setf acc (logand acc (1- (ash 1 nbits))))))
    (when (> nbits 0)
      ;; Pad with EOS (all ones)
      (let ((pad (- 8 nbits)))
        (vector-push-extend
         (logior (ash acc pad) (1- (ash 1 pad)))
         out)))
    out))

;;; String encoding/decoding (RFC 7541 Section 5.2)

(defun hpack-decode-string (data start &optional (end (length data)))
  "Decode HPACK string starting at START.
   Returns (values string bytes-consumed)."
  (unless (< start end)
    (hpack-error "string truncated"))
  (let ((huffman-p (logbitp 7 (aref data start))))
    (multiple-value-bind (slen consumed)
        (hpack-decode-integer data start 7 end)
      (let ((str-start (+ start consumed))
            (str-end (+ start consumed slen)))
        (when (> str-end end)
          (hpack-error "string truncated"))
        (values
         (if huffman-p
             (utf-8-bytes-to-string
              (huffman-decode-bytes data str-start str-end))
             (utf-8-bytes-to-string data :start str-start :end str-end))
         (+ consumed slen))))))

(defun hpack-encode-string (string &key (huffman nil))
  "Encode HPACK string. When HUFFMAN is true, use RFC 7541 Huffman coding."
  (let* ((raw (string-to-utf-8-bytes string))
         (payload (if huffman (huffman-encode-bytes raw) raw))
         (len (length payload))
         (len-bytes (hpack-encode-integer len 7 (if huffman #x80 0)))
         (result (make-array (+ (length len-bytes) len)
                             :element-type '(unsigned-byte 8))))
    (loop for i from 0 below (length len-bytes)
          do (setf (aref result i) (nth i len-bytes)))
    (replace result payload :start1 (length len-bytes))
    result))

;;; Header block decoding (RFC 7541 Section 6)

(defun hpack-decode-headers (ctx data &key (start 0) (end (length data)))
  "Decode HPACK header block.
   Returns list of (name . value) pairs as strings."
  (let ((headers nil)
        (idx start))
    (loop while (< idx end)
          for byte = (aref data idx)
          do (cond
               ;; Indexed Header Field (Section 6.1) - starts with 1
               ((logbitp 7 byte)
                (multiple-value-bind (index consumed)
                    (hpack-decode-integer data idx 7 end)
                  (let ((entry (hpack-lookup-index ctx index)))
                    (push (cons (car entry) (cdr entry)) headers))
                  (incf idx consumed)))

               ;; Literal Header Field with Incremental Indexing (Section 6.2.1) - starts with 01
               ((= (logand byte #xC0) #x40)
                (multiple-value-bind (index consumed)
                    (hpack-decode-integer data idx 6 end)
                  (incf idx consumed)
                  (let (name value)
                    (if (zerop index)
                        (multiple-value-bind (n c)
                            (hpack-decode-string data idx end)
                          (setf name n)
                          (incf idx c))
                        (setf name (car (hpack-lookup-index ctx index))))
                    (multiple-value-bind (v c)
                        (hpack-decode-string data idx end)
                      (setf value v)
                      (incf idx c))
                    (hpack-context-add-entry ctx name value)
                    (push (cons name value) headers))))

               ;; Dynamic Table Size Update (Section 6.3) - starts with 001
               ((= (logand byte #xE0) #x20)
                (multiple-value-bind (size consumed)
                    (hpack-decode-integer data idx 5 end)
                  (hpack-context-update-size ctx size)
                  (incf idx consumed)))

               ;; Literal Header Field without Indexing (Section 6.2.2) - starts with 0000
               ;; Literal Header Field Never Indexed (Section 6.2.3) - starts with 0001
               (t
                (let ((prefix-bits 4))
                  (multiple-value-bind (index consumed)
                      (hpack-decode-integer data idx prefix-bits end)
                    (incf idx consumed)
                    (let (name value)
                      (if (zerop index)
                          (multiple-value-bind (n c)
                              (hpack-decode-string data idx end)
                            (setf name n)
                            (incf idx c))
                          (setf name (car (hpack-lookup-index ctx index))))
                      (multiple-value-bind (v c)
                          (hpack-decode-string data idx end)
                        (setf value v)
                        (incf idx c))
                      (push (cons name value) headers)))))))
    (nreverse headers)))

;;; Header block encoding

(defun hpack-encode-headers (ctx headers)
  "Encode headers to HPACK block.
   HEADERS is a list of (name . value) pairs.
   Uses literal without indexing for simplicity (can be optimized later)."
  (let ((parts nil))
    (dolist (header headers)
      (let* ((name (car header))
             (value (cdr header))
             (name-str (etypecase name
                         (string name)
                         (keyword (string-downcase (symbol-name name)))
                         (symbol (string-downcase (symbol-name name))))))
        ;; Try to find name in static/dynamic table
        (multiple-value-bind (index name-only-p)
            (hpack-find-header ctx name-str value)
          (cond
            ;; Full match - use indexed representation
            ((and index (not name-only-p))
             (let ((bytes (hpack-encode-integer index 7 #x80)))
               (push (make-array (length bytes)
                                 :element-type '(unsigned-byte 8)
                                 :initial-contents bytes)
                     parts)))
            ;; Name match - use literal with indexed name
            (index
             (let* ((name-bytes (hpack-encode-integer index 4 #x00))
                    (value-enc (hpack-encode-string value))
                    (part (make-array (+ (length name-bytes) (length value-enc))
                                      :element-type '(unsigned-byte 8))))
               (loop for i from 0 below (length name-bytes)
                     do (setf (aref part i) (nth i name-bytes)))
               (replace part value-enc :start1 (length name-bytes))
               (push part parts)))
            ;; No match - literal with new name
            (t
             (let* ((name-enc (hpack-encode-string name-str))
                    (value-enc (hpack-encode-string value))
                    (part (make-array (+ 1 (length name-enc) (length value-enc))
                                      :element-type '(unsigned-byte 8))))
               ;; Literal without indexing, new name (0000 0000)
               (setf (aref part 0) #x00)
               (replace part name-enc :start1 1)
               (replace part value-enc :start1 (1+ (length name-enc)))
               (push part parts)))))))
    ;; Concatenate all parts
    (let* ((total-len (reduce #'+ parts :key #'length))
           (result (make-array total-len :element-type '(unsigned-byte 8)))
           (idx 0))
      (dolist (part (nreverse parts))
        (replace result part :start1 idx)
        (incf idx (length part)))
      result)))
