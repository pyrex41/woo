(in-package :cl-user)
(defpackage woo-test.diff
  (:use :cl :rove)
  (:import-from :woo.http2.hpack
                :make-hpack-context
                :hpack-encode-headers
                :hpack-decode-headers
                :hpack-compression-error)
  (:import-from :woo.http2.frames
                :make-data-frame
                :make-headers-frame
                :make-settings-frame
                :make-settings-ack-frame
                :make-window-update-frame
                :make-ping-frame
                :make-ping-ack-frame
                :make-goaway-frame
                :make-rst-stream-frame
                :serialize-frame
                :parse-frame
                :frame-type
                :frame-flags
                :frame-stream-id
                :frame-payload)
  (:import-from :woo.http2.constants
                :+frame-data+
                :+frame-headers+
                :+frame-settings+
                :+frame-window-update+
                :+frame-ping+
                :+frame-goaway+
                :+frame-rst-stream+
                :+no-error+))
(in-package :woo-test.diff)

(defun hex-encode (bytes)
  (with-output-to-string (out)
    (loop for b across bytes
          do (format out "~2,'0X" b))))

(defun hex-decode (text)
  (let* ((n (length text))
         (out (make-array (/ n 2) :element-type '(unsigned-byte 8))))
    (loop for i from 0 below n by 2
          for j from 0
          do (setf (aref out j)
                   (parse-integer text :start i :end (+ i 2) :radix 16)))
    out))

(defun string-bytes (s)
  (trivial-utf-8:string-to-utf-8-bytes s))

(defvar *woo-root*
  (let ((here (or *load-truename* *compile-file-truename*)))
    (dotimes (i 2)
      (setf here (uiop:pathname-parent-directory-pathname here)))
    here))

(defun refh2-dir ()
  (merge-pathnames "t/diff/"
                   (or (ignore-errors (asdf:system-source-directory :woo))
                       *woo-root*)))

(defvar *refh2-bin* nil)

(defun ensure-refh2 ()
  (unless (and *refh2-bin* (probe-file *refh2-bin*))
    (let ((bin (merge-pathnames "woo-refh2" (uiop:temporary-directory))))
      (uiop:run-program
       (list "go" "build" "-o" (namestring bin) ".")
       :directory (refh2-dir)
       :output :string
       :error-output :string)
      (setf *refh2-bin* bin)))
  *refh2-bin*)

(defun refh2 (lines)
  (let* ((input (with-output-to-string (out)
                  (dolist (line lines)
                    (write-line line out))))
         (output (uiop:run-program
                  (list (namestring (ensure-refh2)))
                  :input (make-string-input-stream input)
                  :output :string
                  :error-output :string)))
    (loop for line in (uiop:split-string output :separator '(#\Newline))
          unless (zerop (length line))
          collect line)))

(defun field-line (pairs)
  (with-output-to-string (out)
    (princ "ENC" out)
    (dolist (pair pairs)
      (format out " ~A ~A" (hex-encode (string-bytes (car pair)))
              (hex-encode (string-bytes (cdr pair)))))))

(defun woo-decode (bytes)
  (handler-case
      (hpack-decode-headers (make-hpack-context) bytes)
    (hpack-compression-error () :err)))

(defun pairs-match (expected got)
  (and (listp got)
       (= (length expected) (length got))
       (loop for e in expected
             for g in got
             always (and (string= (car e) (car g))
                         (string= (cdr e) (cdr g))))))

(defun go-decode-line (line)
  (let ((parts (uiop:split-string line :separator '(#\Space))))
    (cond
      ((and (string= (first parts) "DEC") (string= (second parts) "ERR"))
       :err)
      ((string= (first parts) "DEC")
       (loop for rest on (rest parts) by #'cddr
             collect (cons (map 'string #'code-char (hex-decode (first rest)))
                           (map 'string #'code-char (hex-decode (second rest))))))
      (t :bad))))

(deftest differential-hpack-against-go
  (let* ((cases (list '(("custom-key" . "custom-header"))
                      '((":method" . "GET"))
                      '(("a" . "b") ("c" . "d"))
                      '(("www.example.com" . "/") ("accept" . "text/plain"))))
         (enc-lines (mapcar #'field-line cases))
         (go-blocks (refh2 enc-lines)))
    (loop for pairs in cases
          for line in go-blocks
          do (ok (and (>= (length line) 4) (string= (subseq line 0 4) "ENC "))
                 line)
             (let* ((hex (subseq line 4))
                    (bytes (hex-decode hex))
                    (got (woo-decode bytes)))
               (ok (pairs-match pairs got)
                   (format nil "Woo decodes Go's HPACK block as ~S, want ~S" got pairs))))
    (let* ((woo-lines
             (loop for pairs in cases
                   collect (format nil "DEC ~A"
                                   (hex-encode
                                    (hpack-encode-headers (make-hpack-context) pairs)))))
           (decoded (refh2 woo-lines)))
      (loop for pairs in cases
            for line in decoded
            for got = (go-decode-line line)
            do (ok (pairs-match pairs got)
                   (format nil "Go decodes Woo's HPACK block as ~S, want ~S" got pairs))))))

(defun frame-line (frame)
  (format nil "FRM ~A" (hex-encode (serialize-frame frame))))

(defun woo-frame-desc (bytes)
  (multiple-value-bind (frame consumed) (parse-frame bytes)
    (declare (ignore consumed))
    (when frame
      (list (frame-type frame)
            (frame-flags frame)
            (frame-stream-id frame)
            (length (frame-payload frame))))))

(deftest differential-frames-against-go
  (let* ((frames (list (make-data-frame 1 (coerce #(1 2 3) '(simple-array (unsigned-byte 8) (*)))
                                        :end-stream t)
                       (make-headers-frame 1 (coerce #() '(simple-array (unsigned-byte 8) (*)))
                                           :end-headers t :end-stream t)
                       (make-settings-frame nil)
                       (make-settings-ack-frame)
                       (make-window-update-frame 0 128)
                       (make-window-update-frame 1 10)
                       (make-ping-frame (coerce #(1 2 3 4 5 6 7 8)
                                                '(simple-array (unsigned-byte 8) (*))))
                       (make-ping-ack-frame (coerce #(8 7 6 5 4 3 2 1)
                                                    '(simple-array (unsigned-byte 8) (*))))
                       (make-goaway-frame 1 +no-error+)
                       (make-rst-stream-frame 1 +no-error+)))
         (lines (mapcar #'frame-line frames))
         (answers (refh2 lines)))
    (loop for frame in frames
          for bytes = (serialize-frame frame)
          for woo = (woo-frame-desc bytes)
          for line in answers
          do (ok woo)
             (let ((parts (uiop:split-string line :separator '(#\Space))))
               (ok (and (string= (first parts) "FRM") (string= (second parts) "OK"))
                   line)
               (when (and woo (string= (second parts) "OK"))
                 (ok (equal woo
                            (list (parse-integer (third parts))
                                  (parse-integer (fourth parts))
                                  (parse-integer (fifth parts))
                                  (parse-integer (sixth parts))))
                     (format nil "frame fields Woo ~S Go ~S" woo line)))))))
