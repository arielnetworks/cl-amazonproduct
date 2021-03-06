(in-package :cl-amazonproduct)

(cl-interpol:enable-interpol-syntax)

(defun hmac-sha256-digest (key message &key (encoding :utf-8))
  "Return HMAC-SHA256 digest with base64 encoded."
  (let* ((key (babel:string-to-octets key :encoding encoding))
         (message (babel:string-to-octets message :encoding encoding))
         (hmac (ironclad:make-hmac key 'ironclad:sha256)))
    (ironclad:update-hmac hmac message)
    (cl-base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

(defun format-xmldatetime (universal-time &optional time-zone)
  "Return `universal-time' in xml dateTime format."
  (multiple-value-bind (sec min hour day month year day-of-week daylight-saving-time-p time-zone)
      (decode-universal-time universal-time time-zone)
    (declare (ignore day-of-week daylight-saving-time-p time-zone))
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month day hour min sec)))

(declaim (inline string-camelcase))
(defun string-camelcase (val)
  "Return camelcased name."
  (remove #\- (string-capitalize val)))

(declaim (inline string-join))
(defun string-join (delimiter string-list)
  (format nil #?"~{~A~^${delimiter}~}" string-list))

(defun url-safe-char-p (char)
  (find char "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_.-"))

(defun url-escape (string &optional (encoding :utf-8))
  "Return forcely url-escaped string."
  (loop for octet across (babel:string-to-octets string :encoding encoding)
        collect (let ((char (code-char octet)))
                  (if (url-safe-char-p char)
                      (string char)
                      (format nil "%~2,'0X" octet)))
          into lst
        finally (return (apply #'concatenate 'string lst))))

(defun alist-to-url-escaped-string (alist &optional (encoding :utf-8))
  (loop for (name . value) in alist
        collect (format nil "~A=~A" name (url-escape value encoding)) into lst
        finally (return (string-join "&" lst))))
