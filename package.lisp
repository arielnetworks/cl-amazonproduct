(in-package :cl-user)

(defpackage cl-amazonproduct
  (:use :cl :cl-utilities)
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
