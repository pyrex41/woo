(in-package :cl-user)
(defpackage woo.ssl.alpn
  (:use :cl)
  (:import-from :cffi
                :defcfun
                :defcallback
                :foreign-funcall
                :foreign-alloc
                :mem-ref
                :mem-aref
                :with-foreign-object
                :with-foreign-pointer
                :null-pointer-p
                :null-pointer
                :pointer
                :inc-pointer)
  (:import-from :trivial-utf-8
                :string-to-utf-8-bytes)
  (:export :ssl-ctx-set-alpn-select-callback
           :ssl-get0-alpn-selected
           :make-alpn-selector
           :+ssl-tlsext-err-ok+
           :+ssl-tlsext-err-alert-fatal+
           :+ssl-tlsext-err-noack+))
(in-package :woo.ssl.alpn)

;; OpenSSL ALPN callback return values
(defconstant +ssl-tlsext-err-ok+ 0)
(defconstant +ssl-tlsext-err-alert-fatal+ 2)
(defconstant +ssl-tlsext-err-noack+ 3)

;; FFI bindings to OpenSSL ALPN functions
;;
;; SSL_CTX_set_alpn_select_cb:
;; void SSL_CTX_set_alpn_select_cb(SSL_CTX *ctx,
;;                                 int (*cb)(SSL *ssl,
;;                                           const unsigned char **out,
;;                                           unsigned char *outlen,
;;                                           const unsigned char *in,
;;                                           unsigned int inlen,
;;                                           void *arg),
;;                                 void *arg);

(cffi:defcfun ("SSL_CTX_set_alpn_select_cb" %ssl-ctx-set-alpn-select-cb) :void
  (ctx :pointer)
  (cb :pointer)
  (arg :pointer))

;; SSL_get0_alpn_selected:
;; void SSL_get0_alpn_selected(const SSL *ssl,
;;                             const unsigned char **data,
;;                             unsigned int *len);

(cffi:defcfun ("SSL_get0_alpn_selected" %ssl-get0-alpn-selected) :void
  (ssl :pointer)
  (data :pointer)   ; const unsigned char **
  (len :pointer))   ; unsigned int *

(defun ssl-get0-alpn-selected (ssl-handle)
  "Get the ALPN protocol selected during handshake.
   Returns the protocol as a string, or NIL if no protocol was negotiated."
  (cffi:with-foreign-objects ((data-ptr :pointer)
                               (len-ptr :unsigned-int))
    (%ssl-get0-alpn-selected ssl-handle data-ptr len-ptr)
    (let ((data (cffi:mem-ref data-ptr :pointer))
          (len (cffi:mem-ref len-ptr :unsigned-int)))
      (unless (or (cffi:null-pointer-p data) (zerop len))
        (let ((result (make-string len)))
          (dotimes (i len)
            (setf (char result i)
                  (code-char (cffi:mem-aref data :unsigned-char i))))
          result)))))

(defun parse-alpn-protocols (data len)
  "Parse ALPN protocol list from wire format into list of strings.
   Wire format: length-prefixed strings (1 byte length + string bytes).
   Does not read past LEN. A truncated entry ends the list."
  (let ((protocols nil)
        (idx 0))
    (loop while (< idx len)
          do (let ((proto-len (cffi:mem-aref data :unsigned-char idx)))
               (incf idx)
               (unless (<= (+ idx proto-len) len)
                 (return))
               (when (> proto-len 0)
                 (let ((proto (make-string proto-len)))
                   (dotimes (i proto-len)
                     (setf (char proto i)
                           (code-char (cffi:mem-aref data :unsigned-char (+ idx i)))))
                   (push proto protocols)))
               (incf idx proto-len)))
    (nreverse protocols)))

(defun find-protocol-in-buffer (data len protocol)
  "Find a protocol string in the ALPN wire format buffer.
   Returns (values pointer length) if found, NIL otherwise.
   The pointer points directly into the input buffer.
   Does not read past LEN, including when an entry's length byte
   claims more payload than remains."
  (let ((idx 0)
        (proto-len (length protocol)))
    (loop while (< idx len)
          do (let* ((entry-len (cffi:mem-aref data :unsigned-char idx))
                    (entry-end (+ idx 1 entry-len)))
               (unless (<= entry-end len)
                 (return nil))
               (when (= entry-len proto-len)
                 (let ((match t))
                   (dotimes (i proto-len)
                     (unless (char= (char protocol i)
                                    (code-char (cffi:mem-aref data :unsigned-char
                                                              (+ idx 1 i))))
                       (setf match nil)
                       (return)))
                   (when match
                     (return-from find-protocol-in-buffer
                       (values (cffi:inc-pointer data (1+ idx)) proto-len)))))
               (setf idx entry-end)))
    nil))

;; Global variable to hold the ALPN selector function
;; This is needed because the callback doesn't have access to Lisp closures
(defvar *alpn-selector* nil
  "Function that selects ALPN protocol from client's list.
   Should return protocol string or NIL.")

(defvar *preferred-protocols* '("http/1.1")
  "List of preferred ALPN protocols in order of preference.")

(defun protocols-from-arg (arg)
  "Decode length-prefixed protocol list stored as the OpenSSL callback arg."
  (when (and arg (not (cffi:null-pointer-p arg)))
    (handler-case
        (let ((n (cffi:mem-ref arg :unsigned-char)))
          (loop for i from 0 below n
                for off = (+ 1 (* i 32))
                for len = (cffi:mem-aref arg :unsigned-char off)
                collect (let ((s (make-string len)))
                          (dotimes (j len)
                            (setf (char s j)
                                  (code-char (cffi:mem-aref arg :unsigned-char (+ off 1 j)))))
                          s)))
      (error () nil))))

(cffi:defcallback alpn-select-cb :int
    ((ssl :pointer) (out :pointer) (outlen :pointer)
     (in :pointer) (inlen :unsigned-int) (arg :pointer))
  "ALPN selection callback invoked by OpenSSL during TLS handshake."
  (declare (ignore ssl))
  (handler-case
      (let* ((client-protocols (parse-alpn-protocols in inlen))
             (preferred (or (protocols-from-arg arg) *preferred-protocols*)))
        (dolist (proto preferred)
          (when (member proto client-protocols :test #'string=)
            (multiple-value-bind (ptr len)
                (find-protocol-in-buffer in inlen proto)
              (when ptr
                (setf (cffi:mem-ref out :pointer) ptr)
                (setf (cffi:mem-ref outlen :unsigned-char) len)
                (return-from alpn-select-cb +ssl-tlsext-err-ok+)))))
        +ssl-tlsext-err-noack+)
    (error (e)
      (vom:error "ALPN callback error: ~A" e)
      +ssl-tlsext-err-alert-fatal+)))

(defstruct alpn-ctx-arg
  protocols
  ptr
  retired)

;; Per SSL_CTX callback arg. Not one process-global buffer: workers share
;; an SSL_CTX and may be inside alpn-select-cb while another worker
;; reconfigures. Never foreign-free a pointer that has been installed.
(defvar *alpn-ctx-args* (make-hash-table :test 'eql))
(defvar *alpn-arg-lock* (bt2:make-lock))

(defun encode-protocols-arg (preferred-protocols)
  "Allocate a fresh C buffer of PREFERRED-PROTOCOLS. Does not free or
   replace any buffer already handed to OpenSSL."
  (let* ((n (min 8 (length preferred-protocols)))
         (ptr (cffi:foreign-alloc :unsigned-char :count (+ 1 (* 32 n)))))
    (setf (cffi:mem-ref ptr :unsigned-char) n)
    (loop for proto in preferred-protocols
          for i from 0 below n
          for off = (+ 1 (* i 32))
          for len = (min 31 (length proto))
          do (setf (cffi:mem-aref ptr :unsigned-char off) len)
             (dotimes (j len)
               (setf (cffi:mem-aref ptr :unsigned-char (+ off 1 j))
                     (char-code (char proto j)))))
    ptr))

(defun ensure-ctx-alpn-arg (ssl-ctx protocols &key call-openssl)
  "Return the callback arg private to SSL-CTX.
   Reuses the existing buffer when PROTOCOLS is unchanged. A changed list
   allocates a new buffer and retains the previous one so an in-flight
   callback still has a live pointer. CALL-OPENSSL installs it on SSL-CTX."
  (let ((protocols (copy-list protocols)))
    (bt2:with-lock-held (*alpn-arg-lock*)
      (let* ((addr (cffi:pointer-address ssl-ctx))
             (state (gethash addr *alpn-ctx-args*)))
        (cond
          ((and state (equal (alpn-ctx-arg-protocols state) protocols))
           (alpn-ctx-arg-ptr state))
          (t
           (let ((ptr (encode-protocols-arg protocols)))
             (setf (gethash addr *alpn-ctx-args*)
                   (make-alpn-ctx-arg
                    :protocols protocols
                    :ptr ptr
                    :retired (if state
                                 (cons (alpn-ctx-arg-ptr state)
                                       (alpn-ctx-arg-retired state))
                                 nil)))
             (when call-openssl
               (%ssl-ctx-set-alpn-select-cb ssl-ctx
                                             (cffi:callback alpn-select-cb)
                                             ptr))
             ptr)))))))

(defun ssl-ctx-set-alpn-select-callback (ssl-ctx preferred-protocols)
  "Set up ALPN protocol selection on an SSL context.
   PREFERRED-PROTOCOLS is a list of protocol strings in preference order,
   e.g., '(\"h2\" \"http/1.1\"). The callback arg is private to SSL-CTX
   and is not freed while a worker may still be inside the callback."
  (setf *preferred-protocols* (copy-list preferred-protocols))
  (ensure-ctx-alpn-arg ssl-ctx preferred-protocols :call-openssl t))

(defun make-alpn-selector (preferred-protocols)
  "Create an ALPN selector function.
   Returns first matching protocol from client's list, or NIL.
   This is a convenience function - the actual selection uses *preferred-protocols*."
  (lambda (client-protocols)
    (dolist (proto preferred-protocols)
      (when (member proto client-protocols :test #'string=)
        (return proto)))))
