;;;; coleslaw-ui.lisp

(in-package #:coleslaw-ui)


(defvar *blog-dir* #p"~/coleslaw/")

(defvar *stage-dir* #p"/tmp/coleslaw/")

(defvar preview-proc nil)


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


(defvar staged-notification "^[ClW^2^B staged^]")


(defcommand stage-blog () ()
  (bt:make-thread
   (lambda ()
     (uiop:with-current-directory ((truename *blog-dir*))
       (coleslaw-cli:stage)
       (notifications-add staged-notification)
       (message "^[ClW^3^B Blog (re)staged:^2 ~a^]"
                (simple-date-time:|hh:mm| (simple-date-time:now)))))
   :name "stage-blog-thread"))


(defvar preview-notification-on "^[ClW^3^B preview ^2ON^]")

(defvar preview-notification-off "^[ClW^3^B preview ^1OFF^]")


(defcommand start-blog-preview () ()
  (when (null preview-proc)
    (setf preview-proc
          (uiop:launch-program
           (format nil "cd ~a && sbcl --eval \'(ql:quickload :coleslaw-cli)\' --eval \'(coleslaw-cli:preview)\'"
                   *blog-dir*)
           :output :stream :error-output :stream))
    (notifications-add preview-notification-on)
    (notifications-delete preview-notification-off))
  (message preview-notification-on))


(defcommand kill-blog-preview () ()
  (block kill-preview-block
    (when preview-proc
      (uiop:run-program "pkill -f '(coleslaw-cli:preview)'"
                        :ignore-error-status t)
      (uiop:close-streams preview-proc)
      (setf preview-proc nil)
      (notifications-delete preview-notification-on))
    (message preview-notification-off)))


(defvar edit-pub-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") "edit-blog-post")
    (define-key m (kbd "p") "edit-blog-post")
    (define-key m (kbd "a") "edit-blog-page")
    m))


(setf *coleslaw-ui-map*
      (let ((m (make-sparse-keymap)))
        (define-key m (kbd "e") 'edit-pub-map)
        (define-key m (kbd "s") "stage-blog")
        (define-key m (kbd "r") "start-blog-preview")
        (define-key m (kbd "k") "kill-blog-preview")
        m))
