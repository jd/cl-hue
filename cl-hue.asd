(defsystem cl-hue
  :author "Julien Danjou <julien@danjou.info>"
  :description "Client for Philips Hue light controller"
  :license "Apache 2"
  :depends-on (#:drakma
               #:yason
               #:alexandria)
  :components
  ((:file "cl-hue")))
