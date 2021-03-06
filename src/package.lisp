(in-package :cl-user)

(defpackage cl-amazonproduct
  (:nicknames :amazonproduct)
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
  (:export :*aws-associate-tag*
           :*aws-access-key*
           :*aws-secret-key*
           :*aws-locale*
           :*aws-servers*
           :*aws-version*
           :item-lookup
           :item-lookup*
           :item-search
           :item-search*
           :similarity-lookup
           :similarity-lookup*
           :list-lookup
           :list-lookup*
           :list-search
           :list-search*
           :browse-node-lookup
           :browse-node-lookup*
           :cxml-response-handler
           :xmls-response-handler
           :string-response-handler
           :aws-error))
