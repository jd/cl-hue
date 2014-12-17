(defpackage cl-hue
  (:use cl)
  (:export get-devices))

(in-package :cl-hue)

(defconstant +meethue-url+ "http://www.meethue.com/api/nupnp")

(defun get-devices ()
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request +meethue-url+ :want-stream t)
    (cl-json:decode-json stream)))
