;;;; stocks-api.asd

(asdf:defsystem #:stocks-api
  :description "Interface to queries made with Alpha Vantage + RapidApi."
  :author "Vladimir Dikan <vdikan@vivaldi.net>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm #:jonathan)
  :components ((:file "package")
               (:file "stocks-api")))
