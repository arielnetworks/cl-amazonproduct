(in-package :cl-amazonproduct)

(cl-interpol:enable-interpol-syntax)

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

(defun hmac-sha256-digest (key message &key (encoding :utf-8))
  "Return HMAC-SHA256 digest with base64 encoded."
  (let* ((key (babel:string-to-octets key :encoding encoding))
         (message (babel:string-to-octets message :encoding encoding))
         (hmac (ironclad:make-hmac key 'ironclad:sha256)))
    (ironclad:update-hmac hmac message)
    (cl-base64:usb8-array-to-base64-string (ironclad:hmac-digest hmac))))

(defun format-xmldatetime (universal-time &optional time-zone)
  "Return `universal-time' in xml dateTime format."
  (multiple-value-bind (sec min hour day month year day-of-week daylight-saving-time-p time-zone)
      (decode-universal-time universal-time)
    (format nil "~4,'0D-~2,'0D-~2,'0DT~2,'0D:~2,'0D:~2,'0DZ" year month day hour min sec)))

(defun url-escape (string &key (encoding :utf-8))
  "Return forcely url-escaped string."
  (loop for octet across (babel:string-to-octets string :encoding encoding)
        collect (let ((char (code-char octet)))
                  (if (or (alphanumericp char)
                          (member char '(#\_ #\. #\-)))
                      (string char)
                      (format nil "%~2,'0X" octet)))
          into lst
        finally (return (apply #'concatenate 'string lst))))

(defun alist-to-url-escaped-string (alist &key (encoding :utf-8))
  (loop for (name . value) in alist
        collect (format nil "~A=~A" name (url-escape value :encoding encoding)) into lst
        finally (return (string-join "&" lst))))

(defun aws-param-value (object)
  (typecase object
    (list (string-join "," object))
    (symbol (string-camelcase (symbol-name object)))
    (t object)))

(defun aws-request-params (operation params)
  "Return AWS compliant request parameters for `opeartion' on `params'."
  (sort `(("Service" . "AWSECommerceService")
          ("AWSAccessKeyId" . ,*aws-access-key*)
          ("Operation" . ,operation)
          ("Timestamp" . ,(format-xmldatetime (get-universal-time) 0))
          ("Version" . ,*aws-version*)
          ,@params)
        #'string< :key #'car))

(defun aws-request-url (operation params)
  "Setup an AWS compliant request for `operation' on `params' and return a request URL."
  (destructuring-bind (scheme host path) (cdr (assoc *aws-locale* *aws-servers*))
    (let* ((params (aws-request-params operation params))
           (params-string (alist-to-url-escaped-string params))
           (message #?"GET\n${host}\n${path}\n${params-string}")
           (signature (hmac-sha256-digest *aws-secret-key* message))
           (params (cons (cons "Signature" signature) params))
           (params-string (drakma::alist-to-url-encoded-string params :utf-8)))
      #?"${scheme}://${host}${path}?${params-string}")))

(defun aws-request (operation params handler)
  "Request for `opeartion' on `params' and call `handler' with response body."
  (multiple-value-bind (body status-code)
      (drakma:http-request (aws-request-url operation params)
                           :method :get
                           :external-format-out :utf-8)
    (funcall handler body)))

(defun response-cxml-handler (input)
  (cxml:parse input (cxml-dom:make-dom-builder)))

(defun response-xmls-handler (input)
  (cxml:parse input (cxml-xmls:make-xmls-builder)))

(defun response-error-handler (input)
  (multiple-value-bind (match regs)
      (cl-ppcre:scan-to-strings "<Error><Code>([^<]+?)</Code>" input)
    (if match
        (let ((error-code (aref regs 0)))
          (error "AWS Error: ~A" error-code))
        input)))

(defun do-request (operation &rest params)
  (loop with param-alist
        with cxml-handler = #'response-cxml-handler
        with handler = cxml-handler
        for (name value) on params by #'cddr
        do (case name
             (:format
              (case value
                (:cxml (setf handler cxml-handler))
                (:xmls (setf handler #'response-xmls-handler))
                (error "unknown format ~A" value)))
             (t (let ((name (string-camelcase name)))
                  (push (cons name (aws-param-value value)) param-alist))))
        finally (return (aws-request operation
                                     param-alist
                                     (compose handler #'response-error-handler)))))

(defmacro defoperation (name &rest required-params)
  `(defun ,name (,@required-params &rest params)
     (apply #'do-request
            ,(string-camelcase name)
            ,@(loop for param in required-params
                    collect (intern (string-upcase param) :keyword)
                    collect param)
            params)))

(defoperation item-lookup item-id)
(defoperation item-search search-index)
(defoperation similarity-lookup item-id)
(defoperation list-lookup list-id list-type)
(defoperation list-search list-type)
(defoperation browse-node-lookup browse-node-id)
