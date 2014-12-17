(defsystem cl-hue
  :author "Julien Danjou <julien@danjou.info>"
  :description "Client for Philips Hue light controller"
  :depends-on (#:drakma
               #:cl-json)
  :components
  ((:file "cl-hue")))
