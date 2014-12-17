(defpackage cl-hue
  (:use cl)
  (:export get-devices))

(in-package :cl-hue)

(defconstant +meethue-url+ "https://www.meethue.com/api/nupnp")

(defun get-devices ()
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request +meethue-url+ :want-stream t)
    (declare (ignore body status-code headers uri must-close reason-phrase))
    (cl-json:decode-json stream)))


(defun register (bridge-address &optional (device-type "cl-hue"))
  "Register an application against the bridge.

Return a username value (a kind of token) that must be used to access the
bridege."
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request (format nil "http://~a/api" bridge-address)
                           :want-stream t
                           :method :POST
                           :content-type "application/json"
                           :content (cl-json:encode-json-to-string
                                     `((:devicetype . ,device-type))))
    (declare (ignore body status-code headers uri must-close reason-phrase))
    (let* ((status (car (cl-json:decode-json stream)))
           (error-status (cdr (assoc :error status))))
      (if error-status
          (error (cdr (assoc :description error-status)))
          (cdr (assoc :username (cdr (assoc :success status))))))))
