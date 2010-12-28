(in-package :cl-user)

(defpackage cl-amazonproduct-asd
  (:use :cl :asdf))

(in-package :cl-amazonproduct-asd)

(defsystem cl-amazonproduct
  :version "0.1"
  :depends-on (:cl-interpol
               :cl-ppcre
               :cl-utilities
               :cl-base64
               :babel
               :ironclad
               :drakma
               :cxml)
  :components ((:file "package")
               (:file "amazonproduct" :depends-on ("package"))))
