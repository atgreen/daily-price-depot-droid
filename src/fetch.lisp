;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DAILY-PRICE-DEPOT-DROID; Base: 10 -*-
;;;
;;; Copyright (C) 2021, 2024  Anthony Green <anthony@atgreen.org>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU Affero General Public License
;;; as published by the Free Software Foundation, either version 3 of
;;; the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Affero General Public License for more details.
;;;
;;; You should have received a copy of the GNU Affero General Public
;;; License along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>.

(in-package :daily-price-depot-droid)

(defparameter +alphavantage-api-uri+ "https://www.alphavantage.co/query")
(defparameter +goldapi-api-uri+ "https://www.goldapi.io/api")

(defun symbol-currency (equity)
  (let ((epair (split-sequence:split-sequence #\. equity)))
    (if (or (string= (car (cdr epair)) "TSX")
            (string= (car (cdr epair)) "CADFUNDS"))
        "CAD"
        "USD")))

(defun transform-equity-symbol (equity)
  (let ((epair (split-sequence:split-sequence #\. equity)))
    (if (string= (car (cdr epair)) "TSX")
        (concatenate 'string "TSX:" (car epair))
        (car epair))))

(defun fetch-history (equity)
  (format t "Fetching historical data for ~A.~%" equity)
  (sleep 13) ;; Rate limit to 5 calls / minute
  (let ((parameters `(("function" . "TIME_SERIES_DAILY")
                      ("symbol" . ,(transform-equity-symbol equity))
                      ("outputsize" . "full")
                      ("datatype" . "csv")
                      ("apikey" . ,*alphavantage-api-key*))))
    (flexi-streams:octets-to-string
     (drakma:http-request +alphavantage-api-uri+
                          :method :get
                          :parameters parameters))))

(defun fetch-price (equity)
  (format t "Fetching closing price for ~A." equity)
  (sleep 13) ;; Rate limit to 5 calls / minute
  (let ((parameters `(("function" . "GLOBAL_QUOTE")
                      ("symbol" . ,(transform-equity-symbol equity))
                      ("apikey" . ,*alphavantage-api-key*))))
    (flexi-streams:octets-to-string
     (drakma:http-request +alphavantage-api-uri+
                          :method :get
                          :parameters parameters))))

(defun compute-equity-filename (dir sym)
  (concatenate 'string
               dir
               (string-downcase (subseq sym 0 1))
               "/"
               sym
               ".db"))

(defun save-historical-data-for-equity (dir sym)
  (log:info "save-historical-data-for-equity")
  (let ((filename (compute-equity-filename dir sym))
        (cutoff (- (get-universal-time)
                   (- (date-time-parser:parse-date-time "2007")
                      (date-time-parser:parse-date-time "2000")))))
    (unless (probe-file filename)
      (ensure-directories-exist filename)
      (let ((history (fetch-history sym)))
        (handler-case
            (let ((prices (reverse (cdr (cl-csv:read-csv history)))))
              (with-open-file (stream filename :direction :output :if-exists :overwrite :if-does-not-exist :create)
                (dolist (price prices)
                  (let ((date (date-time-parser:parse-date-time (car price))))
                    (when (> date cutoff)
                      (format stream "P ~A 23:59:59 ~A ~A ~A~%"
                              (car price)
                              sym
                              (nth 4 price)
                              (symbol-currency sym)))))))
          (error (c)
            (format t "ERROR: ~A~%" c)
            (format t history)))))))

(defun save-data-for-symbol (dir sym)
  (log:info sym)
  (let ((filename (compute-equity-filename dir sym)))
    (log:info filename)
    (unless (probe-file filename)
      (save-historical-data-for-equity dir sym))
    (let ((price (fetch-price sym)))
      (log:info price)
      (handler-case
          (let ((json (cdar (json:decode-json-from-string price))))
            (print json)
            (with-open-file (stream filename :direction :output :if-exists :append)
              (format stream "P ~A 16:00:00 ~A ~A ~A~%"
                      (cdr (assoc :|07. LATEST TRADING DAY| json))
                      sym
                      (cdr (assoc :|08. PREVIOUS CLOSE| json))
                      (symbol-currency sym))))
        (error (c)
          (format t "ERROR: ~A~%" c)
          (format t price))))))

(defun fetch-forex-history (currency)
  (format t "Fetching historical ~A rates.~%" currency)
  (sleep 13) ;; Rate limit to 5 calls / minute
  (let ((parameters `(("function" . "FX_DAILY")
                      ("from_symbol" . ,currency)
                      ("to_symbol" . "USD")
                      ("outputsize" . "full")
                      ("datatype" . "csv")
                      ("apikey" . ,*alphavantage-api-key*))))
    (flexi-streams:octets-to-string
     (drakma:http-request +alphavantage-api-uri+
                          :method :get
                          :parameters parameters))))

(defun fetch-gold-silver (symbol)
  (format t "Fetching exchange rate for ~A." symbol)
  (sleep 13) ;; Rate limit to 5 calls / minute
  (flexi-streams:octets-to-string
   (drakma:http-request (format nil "~A/~A/USD" +goldapi-api-uri+ symbol)
                        :method :get
                        :additional-headers `(("x-access-token" . ,*goldapi-api-key*)))))

(defun fetch-exchange (currency)
  (format t "Fetching exchange rate for ~A." currency)
  (sleep 13) ;; Rate limit to 5 calls / minute
  (let ((parameters `(("function" . "CURRENCY_EXCHANGE_RATE")
                      ("from_currency" . ,currency)
                      ("to_currency" . "USD")
                      ("datatype" . "csv")
                      ("apikey" . ,*alphavantage-api-key*))))
    (flexi-streams:octets-to-string
     (drakma:http-request +alphavantage-api-uri+
                          :method :get
                          :parameters parameters))))

(defun save-historical-data-for-forex-symbol (dir sym)
  (let ((filename (concatenate 'string dir sym ".db")))
    (unless (probe-file filename)
      (ensure-directories-exist filename)
      (let ((history (fetch-forex-history sym)))
        (handler-case
            (let ((prices (reverse (cdr (cl-csv:read-csv history)))))
              (with-open-file (stream filename :direction :output :if-exists :overwrite :if-does-not-exist :create)
                (dolist (price prices)
                  (format stream "P ~A 23:59:59 ~A ~A USD~%"
                          (car price)
                          sym
                          (cadddr price)))))
          (error (c)
            (format t "ERROR: ~A~%" c)
            (format t history)))))))

(defun save-data-for-forex-symbol (dir sym &key (fetch-history t))
  (let ((filename (concatenate 'string dir sym ".db")))
    (print filename)
    (ensure-directories-exist filename)
    (when fetch-history
      (unless (probe-file filename)
        (save-historical-data-for-forex-symbol dir sym)))
    (let ((exchange (fetch-exchange sym)))
      (handler-case
          (let ((json (cdar (json:decode-json-from-string exchange))))
            (with-open-file (stream filename :direction :output :if-exists :append :if-does-not-exist :create)
              (format stream "P ~A ~A ~A USD~%"
                      (cdr (assoc :|6. *LAST *REFRESHED| json))
                      sym
                      (cdr (assoc :|5. *EXCHANGE *RATE| json)))))
        (error (c)
          (format t "ERROR: ~A~%" c)
          (format t exchange))))))

(defun unix-timestamp-to-date-string (timestamp)
  "Convert a UNIX timestamp to a date-time string."
  (let* ((unix-epoch-to-universal-time-offset 2208988800)
         (universal-time (+ timestamp unix-epoch-to-universal-time-offset))
         (decoded-time (multiple-value-list (decode-universal-time universal-time))))
    (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D"
            (nth 5 decoded-time)  ; year
            (nth 4 decoded-time)  ; month
            (nth 3 decoded-time)  ; day
            (nth 2 decoded-time)  ; hour
            (nth 1 decoded-time)  ; minute
            (nth 0 decoded-time)))) ; second

(defun pull-daily (&optional (config-ini "/etc/daily-price-depot-droid/config.ini"))

  (log:info config-ini)
  (log:info (fad:file-exists-p config-ini))
  (log:info (cl-toml:parse (alexandria:read-file-into-string config-ini :external-format :latin-1)))

  (let ((config (if (fad:file-exists-p config-ini)
	                  (cl-toml:parse
	                   (alexandria:read-file-into-string config-ini
					                                             :external-format :latin-1))
	                (make-hash-table))))

    (flet ((get-config-value (key)
                             (log:info config)
	                           (let ((value (or (gethash key config)
			                                        (error "config does not contain key '~A'" key))))
	                             ;; Some of the users of these values are very strict
	                             ;; when it comes to string types... I'm looking at you,
	                             ;; SB-BSD-SOCKETS:GET-HOST-BY-NAME.
                               (handler-case
		                               (coerce value 'simple-string)
                                 (error () value)))))

      ;; Extract any config.ini settings here.
      (setf *server-uri* (get-config-value "server-uri"))

      (setf *alphavantage-api-key* (get-config-value "ALPHAVANTAGE_API_KEY"))
      (setf *goldapi-api-key* (get-config-value "GOLDAPI_API_KEY"))
      (setf *equities* (get-config-value "equities"))
      (setf *funds* (get-config-value "funds"))
      (setf *fiats* (get-config-value "fiats"))
      (setf *commodities* (get-config-value "commodities"))
      (setf *repo-git-uri* (get-config-value "repo-git-uri"))

      (let ((repo-dir (format nil "~A/daily-price-depot" (uiop:getenv "HOME"))))
        (pull-repo repo-dir *repo-git-uri*)
        (let ((fiat-dir (format nil "~A/daily-price-depot/fiat/" (uiop:getenv "HOME"))))
          (loop for currency across *fiats* do
                (save-data-for-forex-symbol fiat-dir currency)))
        (let ((commodity-dir (format nil "~A/daily-price-depot/commodity/" (uiop:getenv "HOME"))))
          (loop for commodity across *commodities* do
                (if (find commodity '("XAU" "XAG") :test #'string=)
                    (handler-case
                        (let ((json (json:decode-json-from-string (fetch-gold-silver commodity))))
                          (print json)
                          (with-open-file (stream (format nil "~A/daily-price-depot/commodity/~A.db" (uiop:getenv "HOME") commodity) :direction :output :if-exists :append :if-does-not-exist :create)
                                          (format stream "P ~A ~A ~A USD~%"
                                                  (unix-timestamp-to-date-string (cdr (assoc :|TIMESTAMP| json)))
                                                  commodity
                                                  (cdr (assoc :|PRICE| json)))))
                      (error (c)
                             (format t "ERROR: ~A~%" c))))))
        (let ((equity-dir (format nil "~A/daily-price-depot/equity/" (uiop:getenv "HOME"))))
          (loop for equity across *equities* do
                (save-data-for-symbol equity-dir equity)))
        (let ((fund-dir (format nil "~A/daily-price-depot/fund/" (uiop:getenv "HOME"))))
          (loop for fund across *funds* do
                (save-data-for-symbol fund-dir fund)))

        ;; Trim to 7 years max
        (let ((cutoff (- (get-universal-time)
                         (- (date-time-parser:parse-date-time "2007")
                            (date-time-parser:parse-date-time "2000")))))
          (uiop:collect-sub*directories
           (format nil "~A/daily-price-depot" (uiop:getenv "HOME"))
           (constantly t)
           (constantly t)
           (lambda (it)
             (dolist (filename (uiop:directory-files it "*.db"))
               (print filename)
               (let ((trimmed-file (concatenate 'string (namestring filename) ".trim")))
                 (with-open-file (out-stream
                                  trimmed-file :direction :output
                                  :if-exists :overwrite
                                  :if-does-not-exist :create)
                                 (with-open-file (in-stream filename)
                                                 (loop for line = (read-line in-stream nil)
                                                       while line do
                                                       (let ((date (date-time-parser:parse-date-time (subseq line 2 12))))
                                                         (when (> date cutoff)
                                                           (format out-stream "~A~%" line))))))
                 (rename-file trimmed-file filename))))))

        (commit-and-push-repo repo-dir)))))
