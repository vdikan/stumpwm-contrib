;;;; ui-extras.asd

(asdf:defsystem #:ui-extras
  :description "UI enhancements for StumpWM experience"
  :author "Vladimir Dikan"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm)
  :components ((:file "package")
               (:file "ui-extras")))
