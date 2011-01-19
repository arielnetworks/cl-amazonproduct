(in-package :cl-amazonproduct)

(defun cxml-response-handler (input)
  (cxml:parse input (cxml-dom:make-dom-builder)))

(defun xmls-response-handler (input)
  (cxml:parse input (cxml-xmls:make-xmls-builder)))

(defun string-response-handler (input)
  input)

(defun run-response-handler (input handler)
  (funcall handler input))

(defun run-response-error-handler (input)
  (multiple-value-bind (match error-code)
      (cl-ppcre:scan-to-strings "<Error><Code>([^<]+?)</Code>" input)
    (if match
        (error "AWS Error: ~A" (aref error-code 0))
        input)))
