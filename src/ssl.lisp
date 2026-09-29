(defpackage woo.ssl
  (:use :cl)
  (:import-from :cl+ssl
                :with-new-ssl
                :install-nonblock-flag
                :ssl-set-fd
                :ssl-set-accept-state
                :*default-cipher-list*
                :ssl-set-cipher-list
                :with-pem-password
                :install-key-and-cert)
  (:import-from :woo.ev.socket
                :socket-fd
                :socket-ssl-handle)
  (:export :init-ssl-handle))
(in-package :woo.ssl)

;; SSL_set_mode is exposed by OpenSSL as SSL_ctrl and is not exported by all
;; cl+ssl releases. These values are stable across the supported OpenSSL ABI.
(defconstant +ssl-ctrl-mode+ 33)
(defconstant +ssl-mode-enable-partial-write+ #x00000001)
(defconstant +ssl-mode-accept-moving-write-buffer+ #x00000002)

(defun ssl-set-mode (handle mode)
  (cl+ssl::ssl-ctrl handle +ssl-ctrl-mode+ mode (cffi:null-pointer)))

(defun init-ssl-handle (socket ssl-cert-file ssl-key-file ssl-key-password)
  (let ((client-fd (socket-fd socket)))
    (with-new-ssl (handle)
      (install-nonblock-flag client-fd)
      (ssl-set-fd handle client-fd)
      (ssl-set-accept-state handle)
      (when *default-cipher-list*
        (ssl-set-cipher-list handle *default-cipher-list*))
      (ssl-set-mode handle (logior +ssl-mode-enable-partial-write+
                                   +ssl-mode-accept-moving-write-buffer+))
      (setf (socket-ssl-handle socket) handle)
      (with-pem-password ((or ssl-key-password ""))
        (install-key-and-cert
         handle
         ssl-key-file
         ssl-cert-file))
      socket)))
