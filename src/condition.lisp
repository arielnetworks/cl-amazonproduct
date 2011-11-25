(in-package :cl-amazonproduct)

(define-condition aws-error (error)
  ((error-code
    :initarg :error-code
    :reader aws-error-code)
   (error-message
    :initarg :error-message
    :reader aws-error-message))
  (:report (lambda (condition stream)
             (format stream "AWS Error: ~a; ~a"
                     (aws-error-code condition)
                     (aws-error-message condition)))))
