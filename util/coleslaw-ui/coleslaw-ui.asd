;;;; coleslaw-ui.asd

(asdf:defsystem #:coleslaw-ui
  :description "Rudimentary StumpWM UI for the ColeslaW blogware."
  :author "Vladimir Dikan"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:stumpwm
               #:notifications
               #:bt-semaphore
               #:simple-date-time
               #:alexandria
               #:serapeum
               #:cl-arrows
               #:coleslaw-cli)
  :components ((:file "package")
               (:file "coleslaw-ui")))
