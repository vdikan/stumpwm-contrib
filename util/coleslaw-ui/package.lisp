;;;; package.lisp

(defpackage #:coleslaw-ui
  (:use #:cl :stumpwm :notifications :cl-arrows)
  (:export *blog-dir*
           *stage-dir*
           *coleslaw-ui-map*))
