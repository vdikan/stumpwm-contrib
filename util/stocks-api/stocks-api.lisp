;;;; stocks-api.lisp

(in-package #:stocks-api)


(defvar rapidapi-key
  (uiop:getenv "RAPIDAPI_KEY")
  "RapidApi account token. Stored as an environment variable.")


(defvar term-consolep t
  "Use graphics or textual terminal for charting.")


(defcommand stocks-toggle-term-type () ()
  (echo (setf term-consolep (not term-consolep))))


(defvar console-term-width 120
  "Width in chars for the 'dumb terminal.")


(defcommand stocks-set-term-width (width)
    ((:number "Set width in chars for console terminal: "))
  (echo (setf console-term-width width)))


(defvar console-term-height 40
  "Height in chars for the 'dumb terminal.")


(defcommand stocks-set-term-height (height)
    ((:number "Set height in chars for console terminal: "))
  (echo (setf console-term-height height)))


(defvar timefmt "%Y-%m-%d"
  "Time format for plot representation.")


(defvar x-timefmt "%d-%m-%Y"
  "Time format for X-axis display.")


(defvar ticker "DELL"
  "Ticker name for the request.")


(defcommand stocks-set-ticker (new-ticker)
    ((:string "Set Ticker (erases last results): "))
  (setf ticker (string-upcase new-ticker))
  (setf request-results nil)
  (message "Ticker set to ~A. Results wiped." ticker))


(defvar property-names
  '(:open   "1. open"
    :high   "2. high"
    :low    "3. low"
    :close  "4. close"
    :volume "5. volume"))



(defun plotter-cmd ()
  (if term-consolep
      (format nil console-plotter-cmd-template
              ticker (first (getf series-names series))
              timefmt x-timefmt console-term-width console-term-height)
      (format nil graphics-plotter-cmd-template
              ticker (first (getf series-names series)) timefmt x-timefmt)))


(defvar series-names)
(setf series-names
      '(("TIME_SERIES_MONTHLY" . "Monthly Time Series" )
        ("TIME_SERIES_WEEKLY"  . "Weekly Time Series" )
        ("TIME_SERIES_DAILY"   . "Time Series (Daily)" )))
                                        ;FIXME: needs time parsing:
                                        ;:intraday ("Time Series (5min)" "TIME_SERIES_INTRADAY")))


(defcommand stocks-term-info () ()
  (if term-consolep
      (message "Term Type: ^[^2Console^]  Width:^[^3 ~a^]  Height:^[^3 ~a^]"
               console-term-width console-term-height)
      (message "Term Type: ^[^2Graphics^]")))


(defcommand stocks-search-endpoint (input-string)
    ((:string "Search Endpoints for: "))
  (let*
      ((response
         (jonathan:parse
          (uiop:run-program
           (format nil "curl --request GET --url 'https://alpha-vantage.p.rapidapi.com/query?keywords=~a&function=SYMBOL_SEARCH&datatype=json' --header 'x-rapidapi-host: alpha-vantage.p.rapidapi.com' --header 'x-rapidapi-key: ~a'"
                   input-string rapidapi-key)
           :output '(:string :stripped t))
          :as :alist))
       (variants
         (mapcar (lambda (row)
                   (list
                    (format nil "~{~a~^ - ~}"
                            (loop :for (string opt) :on (reverse (alexandria:flatten row))
                                  :by #'cddr :collect string))
                    (first (reverse (alexandria:flatten row))))) (rest (first response))))
       (selected (second (select-from-menu (current-screen) variants))))
    (setf ticker selected)
    (echo selected)))


(defvar stocks-menu)


(defun stocks-prompt ()
  (format nil "=Stocks= Tick:~a "
          ticker
          (when request-results t)))


(defun commandp (command-name)
  "Predicate for StumpWM commands, borrowed from App-Menu."
  (loop
    :for command :being :the :hash-keys :of stumpwm::*command-hash*
    :when (string= (symbol-name command-name) (symbol-name command))
      :return command))


(defcommand display-stocks-menu () ()
  "Show the application menu"
  (labels
      ((pick (options)
         (let ((selection
                 (select-from-menu
                  (current-screen) ; screen
                  options          ; table
                  (stocks-prompt)  ; prompt
                  0)))             ; initial-selection
           (cond
             ((null selection) nil)
             ((symbolp (second selection))
              (if (commandp (second selection))
                  (stumpwm::call-interactively (second selection))
                  (funcall (second selection)))
              (read-one-char (current-screen))
              (pick options))
             (t
              (if (equalp ".." (first selection))
                  (pick (second selection))
                  (pick (append (list (list ".." options))
                                (cdr selection)))))))))
    (pick stocks-menu)))


;; CLOS rewritings
(defvar api-request-queue nil)
(setf api-request-queue nil)


(defclass av-request-base ()
  ((api-function :initarg :api-function
                 :reader api-function
                 :type 'string
                 :allocation :class)
   (request-template :initarg :request-template
                     :reader request-template
                     :type 'string
                     :allocation :class)
   (api-response :initarg api-response
                 :initform nil
                 :type 'string
                 :accessor api-response)
   (api-data :initarg :api-data
             :accessor api-data
             :type 'list)))


(defgeneric results-table (api-request)
  (:documentation "Prints a table using API-DATA from API-REQUEST object."))


(defmethod results-table ((obj av-request-base))
  (format nil "\"~{~{~a ~}~%~}\"" (api-data obj)))


(defgeneric menu-action (api-request)
  (:documentation
   "Logic triggered upon selection of API-REQUEST object from the requests queue. "))


(defmethod menu-action ((obj av-request-base))
  (message "Menu Action Empty"))


(defgeneric make-request (api-request)
  (:documentation
   "Make a request through shell command for an API-REQUEST object."))


(defmethod make-request :around ((obj av-request-base))
  (setf (api-response obj)
        (uiop:run-program
         (request-string obj)
         :output '(:string :stripped t)))
  (push obj api-request-queue)                    ;TODO: обработка ошибки тут
  (when (next-method-p)
    (call-next-method))
  (menu-action obj))


(defclass av-request-extended (av-request-base)
  ((api-meta :initarg :api-meta
             :accessor api-meta
             :type 'list)
   (plotcmd-console :initarg :plotcmd-console
                    :reader plotcmd-console
                    :type 'string
                    :initform  "feedgnuplot --title '~a ~a' --lines --domain --timefmt '~a' --set 'format x \"~a\"' --unset grid --terminal 'dumb ~a,~a' --exit"
                    :allocation :class)
   (plotcmd-graphics :initarg :plotcmd-graphics
                     :reader plotcmd-graphics
                     :type 'string
                     :allocation :class)))


(defgeneric plot-results (api-request)
  (:documentation "Plotter dispatch."))


(defmethod plot-results ((obj av-request-extended))
  ;; (if term-consolep
  (let ((*suppress-echo-timeout* t)
        (*message-window-gravity* :center)
        (plotter-cmd (format nil (plotcmd-console obj)
                             (api-symbol obj)
                             (cdr (assoc  (api-function obj) series-names :test #'string-equal))
                             timefmt x-timefmt console-term-width console-term-height)))
    (message
     (uiop:run-program
      (format nil (concatenate 'string "echo ~a |" plotter-cmd)
              (results-table obj))
      :output '(:string :stripped t))))
  ;; (block graphics-term
  ;;   (bt:make-thread
  ;;    (lambda ()
  ;;      (uiop:run-program
  ;;       (format nil (concatenate 'string "echo ~a |" (plotter-cmd))
  ;;               (results-table))))
  ;;    :name "graphics-term-thread")
  ;;   (message "Output sent to Graphics Terminal")
  ;;   t)
  ;; )
  )


(defun av-request-string-template (string)
  (format nil "curl --request GET --url 'https://alpha-vantage.p.rapidapi.com/query?~a&datatype=json' --header 'x-rapidapi-host: alpha-vantage.p.rapidapi.com' --header 'x-rapidapi-key: ~a'"
          string rapidapi-key))


(defgeneric request-string (api-request)
  (:documentation
   "Construct a whole request shell-command for a certain API-REQUEST."))


(defclass av-search (av-request-base)
  ((api-function :initform "SYMBOL_SEARCH")
   (request-template :initform (av-request-string-template
                                "keywords=~a&function=~a"))
   (api-keywords :initarg :api-keywords :reader api-keywords :type 'string)))


(defmethod request-string ((obj av-search))
  (format nil (request-template obj) (api-keywords obj) (api-function obj)))


(defmethod print-object ((obj av-search) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (api-keywords obj))))


(defmethod make-request ((obj av-search))
  (setf (api-data obj)
        (jonathan:parse (api-response obj)
                        :as :alist)))


(defmethod menu-action ((obj av-search))
  (let*
      ((variants
         (mapcar (lambda (row)
                   (list
                    (format nil "~{~a~^ - ~}"
                            (loop :for (string opt) :on (reverse (alexandria:flatten row))
                                  :by #'cddr :collect string))
                    (first (reverse (alexandria:flatten row)))))
                 (rest (first (api-data obj)))))
       (selected (second (select-from-menu (current-screen) variants))))
    (setf ticker selected)
    (echo selected)))


(defcommand stocks-request-history () ()
  (let* ((variants
           (mapcar (lambda (obj) (cons (format nil "~a" obj) obj))
                   api-request-queue))
         (selected (cdr (select-from-menu (current-screen) variants))))
    (when selected (menu-action selected))))


(defclass av-time-series (av-request-extended)
  ((request-template :initform (av-request-string-template
                                "function=~a&symbol=~a"))
   (api-symbol :initarg :api-symbol
               :reader api-symbol
               :type 'string)))


(defmethod request-string ((obj av-time-series))
  (format nil (request-template obj) (api-function obj) (api-symbol obj)))


(defclass av-series-weekly (av-time-series)
  ((api-function :initform "TIME_SERIES_WEEKLY")))


(defun select-data-property ()
  (alexandria:make-keyword
   (car
    (select-from-menu
     (current-screen)
     (loop :for (key name) :on property-names
           :by #'cddr :collect (string key))
     "Select Property:"))))


(defgeneric select-data-from-response (api-request)
  (:documentation
   "Selects and parses data for certain time property for API-DATA in the API-REQUEST."))


(defmethod select-data-from-response ((obj av-time-series))
  "FIXME: This should build a convenient HIGH-LOW-OPEN-CLOSE table."
  (setf (api-data obj)
        (let ((time-property :close))
          ;;FIXME: (time-property (select-data-property))
          (loop
            :for (date props)
              :on (getf (jonathan:parse (api-response obj) :as :plist)
                        (alexandria:make-keyword
                         (cdr (assoc (api-function obj) series-names :test #'string-equal))))
            :by #'cddr
            :collect (list (string date)
                           (getf props
                                 (alexandria:make-keyword
                                  (getf property-names time-property))))))))


(defmethod make-request ((obj av-time-series))
  (setf (api-meta obj)
        (loop :for (str key)
                :on (reverse
                     (getf (jonathan:parse (api-response ts) :as :plist)
                           :|Meta Data|))
              :by #'cddr
              :collect str))
  (select-data-from-response obj))


(defmethod print-object ((obj av-time-series) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s" (api-symbol obj))))


(defmethod menu-action ((obj av-time-series))
  (plot-results obj))


(setf stocks-menu
      `(("::=[ Stocks Requests History ]=::" stocks-request-history)
        ("Search Endpoint (Ticker)" stocks-search-endpoint) ;; reduce to "compose request"
        ("Set Ticker Manually" stocks-set-ticker)
        ("View Terminal Setting" stocks-term-info)
        ;; ("Toggle Terminal Type" stocks-toggle-term-type)
        ("Set Console Term Width" stocks-set-term-width)
        ("Set Console Term Height" stocks-set-term-height)))


;;;TODO:
;; Menu-Action for time seires;
;; Output of High-Low-Open-Close table;
;; Gnuplot dispatch for graphics plots.


;;; TEST GROUND


(defparameter msft (make-instance 'av-series-weekly :api-symbol "MSFT"))
(defparameter dell (make-instance 'av-series-weekly :api-symbol "DELL"))


(make-request msft)
(make-request dell)
