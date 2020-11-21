;;;; package.lisp

(defpackage #:ui-extras
  (:use #:cl :stumpwm)
  (:import-from #:serapeum :take :drop)
  (:export :display-pager-text))
