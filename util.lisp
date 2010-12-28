(in-package :cl-amazonproduct)

(cl-interpol:enable-interpol-syntax)

(defun string-camelcase (name)
    "Return camelcased name."
    (if (symbolp name)
        (remove #\- (string-capitalize name))
        name))

(defun string-join (delimiter string-list)
  (format nil #?"~{~A~^${delimiter}~}" string-list))
