(in-package :cl-amazonproduct)

(cl-interpol:enable-interpol-syntax)

(defun aws-request-params (operation params)
  "Return AWS compliant request parameters for `opeartion' on `params'."
  (sort `(("Service" . "AWSECommerceService")
          ("AssociateTag" . ,*aws-associate-tag*)
          ("AWSAccessKeyId" . ,*aws-access-key*)
          ("Operation" . ,operation)
          ("Timestamp" . ,(format-xmldatetime (get-universal-time) 0))
          ("Version" . ,*aws-version*)
          ,@params)
        #'string< :key #'car))

(defun aws-request-uri-and-params (operation params)
  "Setup an AWS compliant request for `operation' on `params' and return a request URL."
  (destructuring-bind (scheme host path) (cdr (assoc *aws-locale* *aws-servers*))
    (let* ((params (aws-request-params operation params))
           (params-string (alist-to-url-escaped-string params))
           (message #?"GET\n${host}\n${path}\n${params-string}")
           (signature (hmac-sha256-digest *aws-secret-key* message))
           (uri #?"${scheme}://${host}${path}"))
      (list uri (acons "Signature" signature params)))))

(defun aws-request (operation params)
  "Request for `opeartion' on `params' and call `handler' with response body."
  (destructuring-bind (uri params)
      (aws-request-uri-and-params operation params)
    (drakma:http-request uri
                         :method :get
                         :parameters params
                         :external-format-out :utf-8)))
