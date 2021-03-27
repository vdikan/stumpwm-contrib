;;;; stocks-api.lisp

(in-package #:stocks-api)


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


(defvar x-timefmt "%Y-%m-%d"
  "Time format for X-axis display.")


(defvar api-request-template
  (format nil "curl --request GET --url 'https://alpha-vantage.p.rapidapi.com/query?function=~~a&symbol=~~a&outputsize=compact&datatype=json' --header 'x-rapidapi-host: alpha-vantage.p.rapidapi.com' --header 'x-rapidapi-key: ~a'"
          rapidapi-key))


(defun request-string (function ticker)
  (format nil api-request-template function ticker))


(defvar request-results nil)


(defvar ticker "BTC")


(defcommand stocks-set-ticker (new-ticker)
    ((:string "Set Ticker (erases last results): "))
  (setf ticker (string-upcase new-ticker))
  (setf request-results nil)
  (message "Ticker set to ~A. Results wiped." ticker))


(defparameter series-names
  '(:ts-daily "Time Series (Daily)"))


(defparameter property-names
  '(:open   "1. open"
    :high   "2. high"
    :low    "3. low"
    :close  "4. close"
    :volume "5. volume"))


(setf stocks-menu
      `(("Set Ticker" stocks-set-ticker)
        ("Options.."
         ("Toggle Terminal Type" stocks-toggle-term-type)
         ("Set Console Term Width" stocks-set-term-width)
         ("Set Console Term Height" stocks-set-term-height))))


(defun stocks-prompt ()
  (format nil "Stocks Requests Tick:~a R:~a CT:~a W:~a H:~a"
          ticker
          (when request-results t)
          term-consolep
          console-term-width
          console-term-height))


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
