;;;; taobao.lisp

(in-package :cl-taobao)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utils
(defun YYYYMMDDHHmmss ()
  "Format current time like yyyy-MM-dd HH:mm:ss(2016-04-28 12:09:16). 
  NOTE: top.Util.js exports.YYYYMMDDHHmmss"
  (multiple-value-bind (s m h d mm y) 
      (get-decoded-time) 
    (format nil "~A-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" y mm d h m s)))

(define-condition parameter-missing-error (error)
  ((parameter-required :initarg :parameter-required
                       :reader parameter-required))
  (:report (lambda (condition stream)
             (format stream "'~A' required" (parameter-required condition)))))

(defun check-required (parameter-alist key-list)
  "top.Util.js exports.checkRequired ."
  (unless (listp key-list)
    (setf key-list (list key-list)))
  (loop for key in key-list
     do (unless (assoc key parameter-alist :test #'equal)
          (error 'parameter-missing-error :parameter-required key)))
  t)

(defun get-api-response-name (api-name)
  "NOTE: top.Util.js exports.getApiResponseName"
  (when (ppcre:all-matches "^taobao" api-name)
    (setf api-name (subseq api-name 7)))
  (concatenate 'string
               (ppcre:regex-replace-all "\\." api-name "_")
               "_response"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TopClient
(defclass top-client ()
  ((rest-url :initarg :rest-url :reader rest-url)
   (app-key :initarg :app-key :reader app-key)
   (app-secret :initarg :app-secret :reader app-secret))
  (:documentation "TOP API Client.
  NOTE: topClient.js TopClient"))

(defun make-top-client (app-key app-secret
                        &optional (rest-url "http://gw.api.taobao.com/router/rest"))
  "Make instance of class top-client."
  (make-instance 'top-client
                 :rest-url rest-url
                 :app-key app-key
                 :app-secret app-secret))

(defmethod invoke ((top-client top-client)
                   method parameter-alist response-name-list default-response type callback)
  "Invoke an API by method name.
  NOTE: topClient.js TopClient.prototype.invoke"
  (push (cons "method" method) parameter-alist)
  (request top-client
           parameter-alist
           type ;; request method
           #'(lambda (err result) ;; callback
               (when err
                 (return-from invoke
                   (funcall callback err nil)))
               (let ((response result))
                 (if (and response-name-list
                          (> (length response-name-list) 0))
                     (loop for response-name in response-name-list
                        do (if (jsown:keyp response response-name)
                               (setf response (jsown:val response response-name))
                               (loop-finish))))
                 (unless response
                   (setf response default-response))
                 (funcall callback nil response)))))

(defun wrap-json (string)
  "NOTE: topClient.js TopClient.prototype._wrapJSON"
  (ppcre:regex-replace-all (ppcre:create-scanner "\"id\":\\s?(\\d{16,})"
                                                 :single-line-mode t)
                           string
                           "\"id\":\"\\1\""))

(defvar *ignore-error-codes*
  (jsown:new-js
    ("isv.user-not-exist:invalid-nick" 1))
  "NOTE: topClient.js IGNORE_ERROR_CODES .")

(define-condition jsown-parse-error (error)
  ((data :initarg :data
         :reader data))
  (:report (lambda (condition stream)
             (format stream "data: ~S"
                     (data condition)))))

(defmethod request ((top-client top-client) parameter-alist type callback)
  "Request API.
  NOTE: topClient.js TopClient.prototype.request"
  (when (functionp type) ;; as default, type is request method(:GET/:POST)
    (setf callback type)
    (setf type nil))
  (handler-case (check-required parameter-alist "method")
    (parameter-missing-error (condition)
      (funcall callback condition nil)))
  (let ((arg-list (list (cons "timestamp" (timestamp top-client))
                        (cons "format" "json")
                        (cons "app_key" (app-key top-client))
                        (cons "v" "2.0")
                        (cons "sign_method" "md5"))))
    (loop for (key . value) in parameter-alist
       do (push (cons key (if (stringp value)
                              value
                              (jsown:to-json value)))
                arg-list))
    (push (cons "sign" (sign top-client arg-list))
          arg-list)
    (unless type
      (setf type :GET))
    (let (buffer data result-condition)
      (ignore-errors
        (handler-case
            (multiple-value-bind (data-vector)
                ;; send HTTP request
                (drakma:http-request (rest-url top-client)
                                     :method type
                                     :user-agent nil
                                     :parameters arg-list
                                     :force-binary t)
              (setf buffer data-vector))
          (error (condition)
            (setf result-condition condition))))
      (when buffer
        (setf buffer (babel:octets-to-string buffer :encoding :utf-8)) ;; TODO
        (setf buffer (wrap-json buffer))
        (ignore-errors
          (handler-case (setf data (jsown:parse buffer))
            (error (condition)
              (declare (ignore condition))
              (setf result-condition
                    (make-condition 'jsown-parse-error
                                    :data data))
              (setf data nil)))))
      (when (and data
                 (jsown:keyp data "error_response"))
        (let ((error-response (jsown:val data "error_response")))
          (when (or (not (jsown:keyp error-response "sub_msg"))
                    (not (jsown:keyp *ignore-error-codes*
                                     (if (jsown:keyp error-response
                                                     "sub_code")
                                         (jsown:val error-response "sub_code")
                                         nil))))
            (setf result-condition
                  (make-condition 'top-client-error
                                  :code (jsown:val error-response "code")
                                  :sub-code (if (jsown:keyp error-response
                                                            "sub_code")
                                                (jsown:val error-response "sub_code")
                                                nil)
                                  :data (format nil "~S" data))))))
      (funcall callback result-condition data))))

(defmethod timestamp ((top-client top-client))
  "Get now timestamp with 'yyyy-MM-dd HH:mm:ss' format.
  NOTE: topClient.js TopClient.prototype.timestamp"
  (YYYYMMDDHHmmss))

(defmethod sign ((top-client top-client) parameter-alist)
  "Sign API request. see http://open.taobao.com/doc/detail.htm?id=111#s6
  NOTE: topClient.js TopClient.prototype.sign ."
  (string-upcase
   (ironclad:byte-array-to-hex-string
    (ironclad:digest-sequence :md5
                              (ironclad:ascii-string-to-byte-array 
                               (with-output-to-string (stream)
                                 (format stream (app-secret top-client))
                                 (loop for key in (sort (mapcar #'car
                                                                parameter-alist)
                                                        #'string<)
                                    do (format stream "~A~A"
                                               key
                                               (cdr (assoc key parameter-alist
                                                           :test #'equal))))
                                 (format stream (app-secret top-client))))))))

(define-condition top-client-error (error)
  ((code :initarg :code
         :reader code)
   (sub-code :initarg :sub-code
             :reader sub-code)
   (data :initarg :data
         :reader data))
  (:report (lambda (condition stream)
             (format stream "code: ~A, sub-code: ~A, data: ~S"
                     (code condition)
                     (sub-code condition)
                     (data condition)))))


(defmethod execute ((top-client top-client) api-name parameter-alist callback)
  "Execute top api
NOTE: topClient.js TopClient.prototype.execute"
  (invoke top-client
          api-name
          parameter-alist
          ;; response-name-list
          (list (get-api-response-name api-name))
          ;; default-response
          nil
          ;; type(request-method)
          :post
          ;; callback function
          callback))
