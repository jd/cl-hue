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


(defclass light ()
  ((type :initarg :type :accessor light-type)
   (name :initarg :name :accessor light-name)
   (modelid :initarg :modelid :accessor light-modelid)
   (uniqueid :initarg :uniqueid :accessor light-uniqueid)
   (swversion :initarg :swversion :accessor light-swversion)
   (pointsymbol :initarg :pointsymbol :accessor light-pointsymbol)
   (on :initarg :on :accessor light-on-p)
   (brightness :initarg :brightness :accessor light-brightness)
   (hue :initarg :hue :accessor light-hue)
   (saturation :initarg :saturation :accessor light-saturation)
   (xy :initarg :xy :accessor light-xy)
   (ct :initarg :ct :accessor light-ct)
   (alert :initarg :alert :accessor light-alert)
   (effect :initarg :effect :accessor light-effect)
   (colormode :initarg :colormode :accessor light-colormode)
   (reachable :initarg :reachable :accessor light-reachable-p)))

(defun get-lights (bridge)
  "Get lights status."
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request (format nil "http://~a/api/~a/lights"
                                   (bridge-address bridge)
                                   (bridge-username bridge))
                           :want-stream t)
    (declare (ignore body status-code headers uri must-close reason-phrase))
    (loop for key being the hash-keys of (yason:parse stream)
            using (hash-value value)
          with state
          do (setq state (gethash "state" value))
          collect (make-instance 'light
                                 :type (gethash "type" value)
                                 :name (gethash "name" value)
                                 :modelid (gethash "modelid" value)
                                 :uniqueid (gethash "uniqueid" value)
                                 :swversion (gethash "swversion" value)
                                 :pointsymbol (gethash "pointsymvol" value)
                                 :on (gethash "on" state)
                                 :brightness (gethash "bri" state)
                                 :hue (gethash "hue" state)
                                 :saturation (gethash "sat" state)
                                 :xy (gethash "xy" state)
                                 :ct (gethash "ct" state)
                                 :alert (gethash "alert" state)
                                 :effect (gethash "effect" state)
                                 :colormode (gethash "colormode" state)
                                 :reachable (gethash "reachable" state)))))
