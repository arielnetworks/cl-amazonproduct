(in-package :cl-amazonproduct-test)

;(defun read-parameter (prompt-string)
;  (format t "~&~a: " prompt-string)
;  (loop for line = (read-line)
;        if (not (string= "" line))
;          return line
;        do (format t prompt-string)))
;
;(setf *aws-access-key* (read-parameter "AWS Access Key"))
;(setf *aws-secret-key* (read-parameter "AWS Secret Key"))
(setf *aws-locale* :us)

(plan 1)

(deftest aws-request
    (let ((params (cl-amazonproduct::aws-request-params
                   "ItemLookup"
                   '(("ItemId" . "HOGE")))))
      (is "AWSECommerceService"
          (cdr (assoc "Service" params :test #'string=)))
      (is "HOGE"
          (cdr (assoc "ItemId" params :test #'string=)))))

(run-test-all)
