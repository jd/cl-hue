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
                                     (create-user ip-address)))
        (error "Unable to find a device"))))


(defun extract-api-result (status)
  (let ((error-status (gethash "error" (car status))))
    (if error-status
        (error (gethash "description" error-status))
        (gethash "success" (car status)))))


(defun create-user (bridge-address &optional (device-type "cl-hue") username)
  "Register an application against the bridge.

Return a username value (a kind of token) that must be used to access the
bridege."
  (let* ((payload `("devicetype" ,device-type))
         (payload (if username
                      (append payload `("username" ,username))
                      payload)))
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request (format nil "http://~a/api" bridge-address)
                           :want-stream t
                           :method :POST
                           :content-type "application/json"
                           :content (with-output-to-string (s)
                                        (yason:encode
                                         (alexandria:plist-hash-table payload
                                          :test #'equal)
                                         s)))
    (declare (ignore body status-code headers uri must-close reason-phrase))
    (nth-value 0 (gethash "username" (extract-api-result (yason:parse stream)))))))


(defclass light ()
  ((number :initarg :number :accessor light-number)
   (type :initarg :type :accessor light-type)
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

(defun light-from-status (number status)
  (let ((state (gethash "state" status)))
    (make-instance 'light
                   :number number
                   :type (gethash "type" status)
                   :name (gethash "name" status)
                   :modelid (gethash "modelid" status)
                   :uniqueid (gethash "uniqueid" status)
                   :swversion (gethash "swversion" status)
                   :pointsymbol (gethash "pointsymvol" status)
                   :on (gethash "on" state)
                   :brightness (gethash "bri" state)
                   :hue (gethash "hue" state)
                   :saturation (gethash "sat" state)
                   :xy (gethash "xy" state)
                   :ct (gethash "ct" state)
                   :alert (gethash "alert" state)
                   :effect (gethash "effect" state)
                   :colormode (gethash "colormode" state)
                   :reachable (gethash "reachable" state))))

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
          collect (light-from-status key value))))

(defun get-light (bridge number)
  "Get a specific light."
    (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request (format nil "http://~a/api/~a/lights/~a"
                                   (bridge-address bridge)
                                   (bridge-username bridge)
                                   number)
                           :want-stream t)
      (declare (ignore body status-code headers uri must-close reason-phrase))
      (light-from-status number (yason:parse stream))))


(defgeneric set-light-name (light name)
  (:documentation "Set LIGHT name to NAME."))

(defmethod set-light-name ((light integer) name)
  (multiple-value-bind (body status-code headers uri stream must-close reason-phrase)
      (drakma:http-request (format nil "http://~a/api/~a/lights/~a"
                                   (bridge-address bridge)
                                   (bridge-username bridge)
                                   light)
                           :want-stream t
                           :method :PUT
                           :content-type "application/json"
                           :content (with-output-to-string (s)
                                      (yason:encode
                                       (alexandria:plist-hash-table
                                        `("name" ,name)
                                        :test #'equal)
                                       s)))
    (declare (ignore body status-code headers uri must-close reason-phrase))
    (nth-value 0(gethash (format nil "/lights/~a/name" light)
                         (extract-api-result (yason:parse stream))))))
