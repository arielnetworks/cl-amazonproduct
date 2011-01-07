(in-package :cl-amazonproduct)

(defun check-aws-keys ()
  (or *aws-access-key* (error "Invalid Access Key. Set `*aws-access-key*' first."))
  (or *aws-secret-key* (error "Invalid Secret Key. Set `*aws-secret-key*' first.")))

(defun aws-param-name (key)
  (string-camelcase (symbol-name key)))

(defun aws-param-value (object)
  (typecase object
    (list (string-join "," object))
    (symbol (string-camelcase (symbol-name object)))
    (t object)))

(defun do-request (operation &rest key-pairs &key (xml-builder (cxml-dom:make-dom-builder)) &allow-other-keys)
  (check-aws-keys)
  (setf key-pairs (delete-from-plist key-pairs :xml-builder))
  (multiple-value-bind (body status-code)
      (aws-request operation
                   (loop for (name value) on key-pairs by #'cddr
                         collect (cons (aws-param-name name)
                                       (aws-param-value value))))
    (response-error-handler body)
    (response-handler body xml-builder)))

(defmacro defoperation (name &rest required-params)
  (let ((fun-name (intern (format nil "~A*" name)))
        (param-plist (loop for param in required-params
                           append (list (make-keyword param) param))))
    (with-gensyms (key-pairs additional-params)
      `(progn
         (defun ,fun-name (&rest ,key-pairs &key ,@required-params &allow-other-keys)
           (apply #'do-request
                  ,(string-camelcase name)
                  ,@param-plist
                  (delete-from-plist
                   ,key-pairs
                   ,@(mapcar #'make-keyword required-params))))
         (defun ,name (,@required-params &rest ,additional-params)
           (apply #',fun-name ,@param-plist ,additional-params))))))

(defoperation item-lookup item-id)
(defoperation item-search search-index)
(defoperation similarity-lookup item-id)
(defoperation list-lookup list-id list-type)
(defoperation list-search list-type)
(defoperation browse-node-lookup browse-node-id)
