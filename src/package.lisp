(in-package :cl-user)

(defpackage cl-amazonproduct
  (:use :cl
        :cl-interpol
        :cl-ppcre
        :cl-base64
        :babel
        :ironclad
        :drakma
        :cxml
        :alexandria)
  (:shadowing-import-from :ironclad :null)
  (:shadowing-import-from :babel :string-to-octets)
  (:export :*aws-access-key*
           :*aws-secret-key*
           :*aws-locale*
           :*aws-servers*
           :*aws-version*

           :item-lookup
           :item-search
           :similarity-lookup
           :list-lookup
           :list-search
           :browse-node-lookup
           :lookup))
