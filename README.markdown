cl-amazonproduct
================

cl-amazonproduct is an Amazon Product Advertising API interface library for Common Lisp.

Usage
-----

    ;; Set your access keys.
    (setf *aws-access-key* "YOUR-AWS-ACCESS-KEY")
    (setf *aws-secret-key* "YOUR-AWS-SECRET-KEY")
    (setf *aws-locale* :jp)
    
    ;; Lookup an item with ASIN.
    (item-lookup "4274067211")
    
    ;; Search items with keyword "Common Lisp" in books index.
    (item-search :books :keywords "Common Lisp")

Functions
---------

* item-lookup [item-id]
* item-search [search-index]
* similarity-lookup [item-id]
* list-lookup [list-id list-type]
* list-search [list-type]
* browse-node-lookup [browse-node-id]

License
-------

Copyright (C) 2010 Tomohiro Matsuyama, Eitarow Fukamachi.  
Licensed under the LLGPL License.
