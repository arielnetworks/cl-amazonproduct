(in-package :cl-amazonproduct)

(defun response-cxml-handler (input)
  (cxml:parse input (cxml-dom:make-dom-builder)))

(defun response-xmls-handler (input)
  (cxml:parse input (cxml-xmls:make-xmls-builder)))

(defun response-handler (input result-type)
  (if (eq :xmls result-type)
      (response-xmls-handler input)
      (response-cxml-handler input)))

(defun response-error-handler (input)
  (multiple-value-bind (match error-code)
      (cl-ppcre:scan-to-strings "<Error><Code>([^<]+?)</Code>" input)
    (if match
        (error "AWS Error: ~A" (aref error-code 0))
        input)))
