;;;; ui-extras.lisp

(in-package #:ui-extras)

(defvar *pager-text-num-lines* 20)


(defun make-pager-text-array (text)
  "Split TEXT in lines, return array where each element is a list
of *PAGER-TEXT-NUM-LINES* from the TEXT."
  (let ((input (split-string text '(#\Newline)))
        (result (make-array 8 :fill-pointer 0 :adjustable t)))
    (labels ((push-to-result (list)
               (if list
                   (progn
                     (vector-push-extend (take *pager-text-num-lines* list) result)
                     (push-to-result (drop *pager-text-num-lines* list))))))
      (push-to-result input))
    result))


(defun display-pager-text-page (pager-array page-num)
  "Display PAGE-NUM element from PAGER-ARRAY."
  (let ((page (aref pager-array page-num)))
    (when (consp page)
      (with-message-queuing t
        (message "~{~a~%~}" page)
        (message "^[^6====::::...^]^>^[^6::::===^]")
        (message "^[^B^3[ Pager ]  ^b^nNavigate: ^5n/p ^nor ^5Space/BSpace ^]^>^[^B^6 [~a/~a]^]"
                 (+ 1 page-num) (length pager-array))))))


(defun display-pager-text-array (pager-array)
  "Display PAGER-ARRAY through the paging UI logic."
  (let ((current-page-num 0))
    (labels ((process-input ()
               (let ((control-char (read-one-char (current-screen))))
                 (if (characterp control-char)
                     (cond
                       ((or (char-equal control-char #\n)
                            (char-equal control-char #\Space))
                        (progn
                          (if (< current-page-num (- (length pager-array) 1))
                              (incf current-page-num))
                          (display-page)))
                       ((or (char-equal control-char #\p)
                            (char-equal control-char #\Backspace))
                        (progn
                          (if (> current-page-num 0)
                              (decf current-page-num))
                          (display-page)))
                       (t (message "Stopped Paging")))
                     (message "Stopped Paging"))))
             (display-page ()
               (progn
                 (let ((*suppress-echo-timeout* t)
                       (*message-window-gravity* :center))
                   (display-pager-text-page pager-array current-page-num))
                 (process-input))))
      (display-page))))


(defun display-pager-text (text)
  "Display (long) TEXT string through pager UI. TEXT is split by lines;
 each page shows number of lines set in *PAGER-TEXT-NUM-LINES*."
  (display-pager-text-array
   (make-pager-text-array text)))
