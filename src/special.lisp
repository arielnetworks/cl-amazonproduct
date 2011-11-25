(in-package :cl-amazonproduct)

(defparameter *aws-associate-tag* nil)
(defparameter *aws-access-key* nil)
(defparameter *aws-secret-key* nil)
(defparameter *aws-locale* :us)
(defparameter *aws-servers*
              '((:ca "http" "ecs.amazonaws.ca" "/onca/xml")
                (:de "http" "ecs.amazonaws.de" "/onca/xml")
                (:fr "http" "ecs.amazonaws.fr" "/onca/xml")
                (:jp "http" "ecs.amazonaws.jp" "/onca/xml")
                (:uk "http" "ecs.amazonaws.co.uk" "/onca/xml")
                (:us "http" "ecs.amazonaws.com" "/onca/xml")))
(defparameter *aws-version* "2009-11-01")
