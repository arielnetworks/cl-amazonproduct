(in-package :cl-amazonproduct)

(cl-interpol:enable-interpol-syntax)

(defun string-camelcase (val)
  "Return camelcased name."
  (remove #\- (string-capitalize val)))

(defun string-join (delimiter string-list)
  (format nil #?"~{~A~^${delimiter}~}" string-list))
