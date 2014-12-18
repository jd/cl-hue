(defpackage cl-hue
  (:use cl)
  (:export get-devices))

(in-package :cl-hue)

(defconstant +meethue-url+ "https://www.meethue.com/api/nupnp")

(defun get-devices ()
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request +meethue-url+ :want-stream t)
    (declare (ignore body status-code headers uri must-close reason-phrase))
    (yason:parse stream)))


(defclass bridge ()
  ((address :initarg :address :accessor bridge-address)
   (username :initarg :username :accessor bridge-username)))

(defun make-bridge (&optional ip-address username)
  (let ((ip-address (or ip-address
                        ;; Grab the first device in the list
                        (cdr (assoc :internalipaddress (car (get-devices)))))))
    (if ip-address
        (make-instance 'bridge
                       :address ip-address
                       :username (or username
                                     (register ip-address)))
        (error "Unable to find a device"))))


(defun register (bridge-address &optional (device-type "cl-hue"))
  "Register an application against the bridge.

Return a username value (a kind of token) that must be used to access the
bridege."
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request (format nil "http://~a/api" bridge-address)
                           :want-stream t
                           :method :POST
                           :content-type "application/json"
                           :content (with-output-to-string (s)
                                        (yason:encode
                                         (alexandria:plist-hash-table
                                          `("devicetype" ,device-type)
                                          :test #'equal)
                                         s)))
    (declare (ignore body status-code headers uri must-close reason-phrase))
    (let* ((status (car (yason:parse stream)))
           (error-status (gethash "error" status)))
      (if error-status
          (error (gethash "description" error-status))
          (nth-value 0 (gethash "username" (gethash "success" status)))))))
