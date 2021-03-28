;;;; stocks-api.lisp

(in-package #:stocks-api)

(ql:quickload :jonathan)


(defvar rapidapi-key
  ;; (uiop:getenv "RAPIDAPI_KEY")
  "RapidApi account token. TODO: get as environment var")


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


(defvar ticker "BTC"
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


(defvar property :close
  "Keyword for selecting property from request.")


(defcommand stocks-set-property () ()
  (setf property
        (alexandria:make-keyword
         (let ((completion-names
                 (loop :for (key name) :on property-names
                       :by #'cddr :collect (string key))))
           (completing-read
            (current-screen)
            "Select property in response: "
            completion-names
            :initial-input (string property)
            :require-match t))))
  (echo property))


(defvar console-plotter-cmd-template
  "feedgnuplot --title '~a' --lines --domain --timefmt '~a' --set 'format x \"~a\"' --unset grid --terminal 'dumb ~a,~a' --exit")


(defvar graphics-plotter-cmd-template
  "feedgnuplot --lines --domain --timefmt '~a' --set 'format x \"~a\"'")


(defun plotter-cmd ()
  (if term-consolep
      (format nil console-plotter-cmd-template
              ticker timefmt x-timefmt console-term-width console-term-height)
      (format nil graphics-plotter-cmd-template timefmt x-timefmt)))


(defvar request-results nil
  "Stores the final data table (X VAL) for the a last processed request.")


(defvar api-request-template
  (format nil "curl --request GET --url 'https://alpha-vantage.p.rapidapi.com/query?function=~~a&symbol=~~a&outputsize=compact&datatype=json' --header 'x-rapidapi-host: alpha-vantage.p.rapidapi.com' --header 'x-rapidapi-key: ~a'"
          rapidapi-key))


(defun request-string (function ticker)
  (format nil api-request-template function ticker))


(defvar series-names
  '(:daily ("Time Series (Daily)" "TIME_SERIES_DAILY")))


(defvar series :daily)


(defun make-request ()
  (let* ((request
           (request-string (second (getf series-names series)) ticker))
         (response
           (uiop:run-program request :output '(:string :stripped t))))
    response))


(defun process-request ()
  (let ((response (make-request)))
    (when response
      (setf request-results
            (loop
              :for (date props)
                :on (getf (jonathan:parse response :as :plist)
                          (alexandria:make-keyword
                           (first (getf series-names series))))
              :by #'cddr
              :append (list (string date)
                            (getf props
                                  (alexandria:make-keyword
                                   (getf property-names property)))))))))


(defun results-table ()
  (format nil "\"~{~a ~a~%~}\"" request-results))


(defun plot-results ()
  (if term-consolep
      (let ((*suppress-echo-timeout* t)
            (*message-window-gravity* :center))
        (message
         (uiop:run-program
          (format nil (concatenate 'string "echo ~a |" (plotter-cmd))
                  (results-table))
          :output '(:string :stripped t))))
      (let ((graphics t))
        ;; FIXME: aproc tread here
        (uiop:launch-program
         (format nil (concatenate 'string "echo ~a |" (plotter-cmd))
                 (results-table)))
        (message "Output to Graphics Terminal")
        graphics)))


(defcommand stocks-plot-results () ()
  (if request-results
      (plot-results)
      (message "^[^1No Data Stored!^]")))


(defcommand stocks-make-request () ()
  (make-request)
  (when (process-request) (plot-results)))


(defcommand stocks-term-info () ()
  (if term-consolep
      (message "Term Type: ^[^2Console^]  Width:^[^3 ~a^]  Height:^[^3 ~a^]"
               console-term-width console-term-height)
      (message "Term Type: ^[^2Graphics^]")))


(defvar stocks-menu)
(setf stocks-menu
      `(("Replot Stored Data" stocks-plot-results)
        ("Request Selected Ticker Data" stocks-make-request)
        ("Set Ticker" stocks-set-ticker)
        ("View Terminal Setting" stocks-term-info)
        ("Options.."
         ("Set Request Property" stocks-set-property)
         ("Toggle Terminal Type" stocks-toggle-term-type)
         ("Set Console Term Width" stocks-set-term-width)
         ("Set Console Term Height" stocks-set-term-height))))


(defun stocks-prompt ()
  (format nil "=Stocks= Tick:~a Prop:~a Ser:~a Resp:~a"
          ticker
          property
          series
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
