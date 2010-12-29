(in-package :cl-user)

(defpackage cl-amazonproduct-test-asd
  (:use :cl :asdf))

(in-package :cl-amazonproduct-test-asd)

(defsystem cl-amazonproduct-test
  :depends-on (:cl-amazonproduct :fiveam)
  :components ((:module "test"
                :serial t
                :components ((:file "package")
                             (:file "test")))))
