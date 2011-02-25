(in-package :cl-amazonproduct)

(define-condition aws-error (error)
  ((error-code
    :initarg :error-code
    :reader aws-error-code))
  (:report (lambda (condition stream)
             (format stream "AWS Error: ~a"
                     (aws-error-code condition)))))
