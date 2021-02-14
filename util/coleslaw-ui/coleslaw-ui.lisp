;;;; coleslaw-ui.lisp

(in-package #:coleslaw-ui)


(defvar *blog-dir* #p"~/coleslaw/")

(defvar *stage-dir* #p"/tmp/coleslaw/")

(defvar preview-proc nil)


(defun check-pub-type (file type)
  (string-equal type (pathname-type file)))


(defun publications-list (type)
  (let ((files-list
          (remove-if-not
           (lambda (pub) (check-pub-type pub type))
           (uiop:directory-files *blog-dir*))))
    (-> (mapcar #'pathname-name files-list)
        (reverse))))


(defun pub-truename (name type)
  (uiop:with-current-directory (*blog-dir*)
    (namestring (truename (format nil "~a.~a" name type)))))


(defun ui-pub-menu (type action)
  (let ((stumpwm::*menu-maximum-height* 12))
    (let ((pub-name
            (select-from-menu
             (current-screen)
             (publications-list type)
             (format nil "^>^[^6..::::====[^B ~a ~a ^b]==::'^]" action type))))
      (when pub-name (pub-truename (first pub-name) type)))))


(defun edit-pub (filename)
  "Edit publication in emacs."
  (run-or-raise
   (concatenate 'string "exec emacsclient " filename)
   '(:title "edit publication")))


(defun edit-pub-menu (type)
  (let ((pub (ui-pub-menu type "Edit")))
    (if (not (null pub))
        (edit-pub pub)
        (echo "^B^1Cancel"))))


(defcommand edit-blog-post () ()
  (edit-pub-menu "post"))


(defcommand edit-blog-page () ()
  (edit-pub-menu "page"))


(defun create-pub (type &optional prepend-timestamp)
  (let ((new-pub-name
          (read-one-line
           (current-screen)
           (format nil "New ~a filename:" type)
           :initial-input (if prepend-timestamp
                              (format nil "~a-" (simple-date-time:yyyy-mm-dd
                                                 (simple-date-time:now)))
                              ""))))
    (when new-pub-name
      (uiop:with-current-directory
          (*blog-dir*)
        (coleslaw-cli:new type new-pub-name)
        (edit-pub (namestring (truename (format nil "~a.~a" new-pub-name type))))))))


(defcommand create-blog-post () ()
  (create-pub "post" t))


(defcommand create-blog-page () ()
  (create-pub "page"))


(defun preview-pub (filename)
  "Try open FILENAME publication preview in browser.
Somewhat clumsy...Need to refactor namesss"
  (if (null preview-proc) (start-blog-preview))
  (with-open-file (stream filename)
    (read-line stream nil)
    (uiop:run-program
     (format nil "sensible-browser http://localhost:5000/~a~a.html"
             (if (string-equal (pathname-type filename) "post") "posts/" "")
             (if (string-equal (pathname-type filename) "post")
                 (string-downcase
                  (format nil "~{~a~^-~}"
                          (-<> (read-line stream nil)
                               (remove-if (lambda (c) (char-equal #\' c)) <>)
                               (split-string <> '(#\Space #\# #\:))
                               (subseq <> 1))))
                 (pathname-name filename))))))


(defun preview-pub-menu (type)
  (let ((pub (ui-pub-menu type "Preview in Browser")))
    (if (not (null pub))
        (preview-pub pub)
        (echo "^B^1Cancel"))))


(defcommand preview-blog-post () ()
  (preview-pub-menu  "post"))


(defcommand preview-blog-page () ()
  (preview-pub-menu  "page"))


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


(defvar create-pub-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "c") "create-blog-post")
    (define-key m (kbd "p") "create-blog-post")
    (define-key m (kbd "a") "create-blog-page")
    m))


(defvar preview-pub-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "p") "preview-blog-post")
    (define-key m (kbd "a") "preview-blog-page")
    m))


(defcommand edit-blog-rc () ()
  (uiop:with-current-directory ((truename *blog-dir*))
    (run-or-raise
     (concatenate 'string "exec emacsclient "
                  (namestring (truename #p".coleslawrc")))
     '(:title "edit coleslawrc"))))


(defvar deployed-notification "^[ClW^2^B deployed!^]")


(defcommand deploy-blog () ()
  (bt:make-thread
   (lambda ()
     (uiop:run-program
      (format nil "cd ~a && sbcl --eval \'(ql:quickload :coleslaw-cli)\' --eval \'(coleslaw-cli:deploy)\'"
              *blog-dir*))
     (notifications-add deployed-notification)
     (message deployed-notification))
   :name "deploy-blog-thread"))


(defvar *coleslaw-ui-map*
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "e") 'edit-pub-map)
    (define-key m (kbd "c") 'create-pub-map)
    (define-key m (kbd "p") 'preview-pub-map)
    (define-key m (kbd "s") "stage-blog")
    (define-key m (kbd "r") "start-blog-preview")
    (define-key m (kbd "k") "kill-blog-preview")
    (define-key m (kbd "o") "edit-blog-rc")
    (define-key m (kbd "D") "deploy-blog")
    m))
