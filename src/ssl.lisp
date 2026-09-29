(defpackage woo.ssl
  (:use :cl)
  (:import-from :cl+ssl
                :with-new-ssl
                :install-nonblock-flag
                :ssl-set-fd
                :ssl-set-accept-state
                :*default-cipher-list*
                :ssl-set-cipher-list
                :ssl-ctx-free)
  (:import-from :woo.ev.socket
                :socket-fd
                :socket-ssl-handle)
  (:import-from :woo.ssl.alpn
                :ssl-ctx-set-alpn-select-callback
                :release-ctx-alpn
                :ssl-get0-alpn-selected)
  (:export :create-context
           :free-context
           :init-ssl-handle
           :get-negotiated-protocol
           :*alpn-protocols*
           :configure-alpn
           :configure-context-alpn))
(in-package :woo.ssl)

;; SSL_set_mode is exposed by OpenSSL as SSL_ctrl and is not exported by all
;; cl+ssl releases. These values are stable across the supported OpenSSL ABI.
(defconstant +ssl-ctrl-mode+ 33)
(defconstant +ssl-mode-enable-partial-write+ #x00000001)
(defconstant +ssl-mode-accept-moving-write-buffer+ #x00000002)

(defun ssl-set-mode (handle mode)
  (cl+ssl::ssl-ctrl handle +ssl-ctrl-mode+ mode (cffi:null-pointer)))

(defvar *alpn-protocols* '("http/1.1")
  "List of ALPN protocols to advertise, in preference order.
   Set to '(\"h2\" \"http/1.1\") to enable HTTP/2.
   Default is HTTP/1.1 only for backward compatibility.")

(defvar *alpn-configured-p* nil
  "Whether ALPN has been configured on the current SSL context.")

(defun configure-context-alpn (ssl-ctx &optional (protocols *alpn-protocols*))
  "Configure ALPN on SSL-CTX with PROTOCOLS (preference order).
   The callback argument is owned by that SSL_CTX and remains live until the
   context is freed.  Context-local configuration prevents one listener from
   changing another listener's protocol selection."
  (handler-case
      (when ssl-ctx
        (ssl-ctx-set-alpn-select-callback ssl-ctx protocols)
        (setf *alpn-configured-p* t)
        (vom:info "ALPN configured with protocols: ~A" protocols))
    (error (e)
      (vom:warn "Failed to configure ALPN: ~A" e))))

(defun configure-alpn (&optional (protocols *alpn-protocols*))
  "Configure ALPN on the legacy global context.
   Listener startup uses CONFIGURE-CONTEXT-ALPN so contexts remain isolated."
  (setf *alpn-protocols* (copy-list protocols))
  (when cl+ssl::*ssl-global-context*
    (configure-context-alpn cl+ssl::*ssl-global-context* protocols)))

(cffi:defcfun ("SSL_CTX_check_private_key" %ssl-ctx-check-private-key) :int
  (context :pointer))

(defun create-context (ssl-cert-file ssl-key-file ssl-key-password)
  "Create a listener-owned context and load the complete PEM chain."
  (let ((context (cl+ssl:make-context
                  :certificate-chain-file (and ssl-cert-file (uiop:native-namestring ssl-cert-file))
                  :private-key-file (and ssl-key-file (uiop:native-namestring ssl-key-file))
                  :private-key-password ssl-key-password
                  :verify-mode cl+ssl:+ssl-verify-none+))
        (valid nil))
    (unwind-protect
         (progn
           (unless (= 1 (%ssl-ctx-check-private-key context))
             (error "TLS certificate and private key do not match or could not be loaded"))
           (setf valid t)
           context)
      (unless valid (ssl-ctx-free context)))))

(defun init-ssl-handle (socket ssl-ctx ssl-cert-file ssl-key-file ssl-key-password)
  "Initialize SSL handle for a socket.
   Sets up TLS with the provided certificate and key.
   The context is owned by the listener and outlives this connection."
  (declare (ignore ssl-cert-file ssl-key-file ssl-key-password))
  (let ((client-fd (socket-fd socket)))
    (cl+ssl:with-global-context (ssl-ctx)
      (with-new-ssl (handle)
        (install-nonblock-flag client-fd)
        (ssl-set-fd handle client-fd)
        (ssl-set-accept-state handle)
        (when *default-cipher-list*
          (ssl-set-cipher-list handle *default-cipher-list*))
        ;; SSL_set_mode is a compatibility-sensitive FFI detail.  The
        ;; stream retry path keeps the bytes stable, so these modes are safe
        ;; and allow OpenSSL to report progress on nonblocking writes.
        (ssl-set-mode handle (logior +ssl-mode-enable-partial-write+
                                     +ssl-mode-accept-moving-write-buffer+))
        (setf (socket-ssl-handle socket) handle)
        socket))))

(defun free-context (ssl-ctx)
  (when ssl-ctx
    (release-ctx-alpn ssl-ctx)
    (ssl-ctx-free ssl-ctx)))

(defun get-negotiated-protocol (socket)
  "Get the ALPN-negotiated protocol for this socket.
   Returns the protocol string (e.g., \"h2\" or \"http/1.1\") or NIL
   if no protocol was negotiated (e.g., client doesn't support ALPN)."
  (let ((ssl-handle (socket-ssl-handle socket)))
    (when ssl-handle
      (ssl-get0-alpn-selected ssl-handle))))
