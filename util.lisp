(in-package :cl-amazonproduct)

(cl-interpol:enable-interpol-syntax)

(declaim (inline string-camelcase))
(defun string-camelcase (val)
  (declare (optimize speed)
           (type string val))
  "Return camelcased name."
  (the string (remove #\- (string-capitalize val))))

(declaim (inline string-join))
(defun string-join (delimiter string-list)
  (declare (optimize speed)
           (type character delimiter)
           (type list string-list))
  (the string (format nil #?"~{~A~^${delimiter}~}" string-list)))
