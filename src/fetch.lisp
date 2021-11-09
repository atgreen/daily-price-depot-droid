;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DAILY-PRICE-DEPOT-DROID; Base: 10 -*-
;;;
;;; Copyright (C) 2021  Anthony Green <anthony@atgreen.org>
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
(defparameter *alphavantage-api-key* (uiop:getenv "ALPHAVANTAGE_API_KEY"))

(defun equity-currency (equity)
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

(defun fetch-price (equity)
  (format t "Fetching closing price for ~A." equity)
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

(defun save-data-for-equity (dir sym)
  (let ((filename (compute-equity-filename dir sym)))
    (print filename)
    (when (probe-file filename)
      (sleep 13) ;; Rate limit to 5 requests per minute per API key
      (let ((price (fetch-price sym)))
        (print price)
        (handler-case
            (let ((json (cdar (json:decode-json-from-string price))))
              (print json)
              (with-open-file (stream filename :direction :output :if-exists :append)
                (format stream "P ~A 16:00:00 ~A ~A ~A~%"
                        (cdr (assoc :|07. LATEST TRADING DAY| json))
                        sym
                        (cdr (assoc :|08. PREVIOUS CLOSE| json))
                        (equity-currency sym))))
          (error (c)
            (format t "ERROR: ~A~%" c)
            (format t price)))))))

(defun pull-daily ()
  (let ((equity-dir (format nil "~A/daily-price-depot/equity/" (uiop:getenv "HOME"))))
    (loop for equity across *equities* do
      (save-data-for-equity equity-dir equity))
    (commit-and-push-repo equity-dir)))
