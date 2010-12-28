# cl-amazonproduct - Common Lisp Interface to Amazon Product API

## Usage

    ;; Set your access keys.
    (setf *aws-access-key* "YOUR-AWS-ACCESS-KEY")
    (setf *aws-secret-key* "YOUR-AWS-SECRET-KEY")
    (setf *aws-locale* :jp)
    
    (item-lookup :item-id "4274067211")
    (item-search :search-index :books :keywords "Common Lisp")

## Functions

* item-lookup [item-id]
* item-search [search-index]
* similarity-lookup [item-id]
* list-lookup [list-id list-type]
* list-search [list-type]
* browse-node-lookup [browse-node-id]

## License

Copyright (C) 2010 Tomohiro Matsuyama, Eitarow Fukamachi.  
Licensed under the LLGPL License.
