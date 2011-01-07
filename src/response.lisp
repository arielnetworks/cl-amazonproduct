(in-package :cl-amazonproduct)

(defun response-handler (input xml-builder)
  (cxml:parse input xml-builder))

(defun response-error-handler (input)
  (multiple-value-bind (match error-code)
      (cl-ppcre:scan-to-strings "<Error><Code>([^<]+?)</Code>" input)
    (if match
        (error "AWS Error: ~A" (aref error-code 0))
        input)))
