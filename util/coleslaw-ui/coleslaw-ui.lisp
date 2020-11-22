;;;; coleslaw-ui.lisp

(in-package #:coleslaw-ui)


(defvar *blog-dir* #p"~/coleslaw/")

(defvar *stage-dir* #p"/tmp/coleslaw/")


(defun check-pub-type (file type)
  (string-equal type (pathname-type file)))


(defun publications-alist (type)
  (let ((files-list
          (remove-if-not
           (lambda (pub) (check-pub-type pub type))
           (uiop:directory-files *blog-dir*))))
    (-> (mapcar (lambda (pub) (cons (pathname-name pub) (namestring pub)))
                files-list)
        (reverse))))


(defun edit-pub (filename)
  "Edit publication in emacs."
  (run-or-raise
   (concatenate 'string "exec emacsclient " filename)
   '(:title "edit publication")))


(defun ui-pub-menu (type action)
  (let ((stumpwm::*menu-maximum-height* 12))
    (select-from-menu
     (current-screen)
     (publications-alist type)
     (format nil "^>^[^6..::::====[^B ~a ~a ^b]==::'^]" action type))))


(defun edit-pub-menu (type)
  (let ((pub (ui-pub-menu type "Edit")))
    (if (not (null pub))
        (edit-pub (cdr pub))
        (echo "^B^1Cancel"))))


(defcommand edit-blog-post () ()
  (edit-pub-menu "post"))


(defcommand edit-blog-page () ()
  (edit-pub-menu "page"))


(defvar edit-pub-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") "edit-blog-post")
    (define-key m (kbd "p") "edit-blog-post")
    (define-key m (kbd "a") "edit-blog-page")
    m))


(defvar *coleslaw-ui-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") 'edit-pub-map)
    m))
