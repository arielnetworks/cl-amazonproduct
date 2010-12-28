(in-package :cl-user)

(defpackage cl-amazonproduct
  (:use :cl
        :cl-utilities
        :cl-interpol
        :cl-ppcre
        :cl-base64
        :babel
        :ironclad
        :drakma
        :cxml)
  (:shadowing-import-from :ironclad :expt-mod :null)
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
           :browse-node-lookup))
