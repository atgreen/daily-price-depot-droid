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

;; Top level for daily-price-depot-droid

(markup:enable-reader)

(in-package :daily-price-depot-droid)

;; ----------------------------------------------------------------------------
;; Get the version number at compile time.  This comes from
;; APP_VERSION (set on the linux container build commandline), or from
;; git at compile-time.  Use UNKNOWN if all else fails.

;; This can come from build time...
(eval-when (:compile-toplevel :execute :load-toplevel)
  (defparameter +daily-price-depot-droid-git-version+
    (inferior-shell:run/ss
     "git describe --tags --dirty=+ || git rev-parse --short HEAD || echo UNKNOWN")))

;; But this must come from runtime...
(defparameter +daily-price-depot-droid-version+
  (let ((v +daily-price-depot-droid-git-version+))
    (if (equal v "UNKNOWN")
 	(or (uiop:getenv "APP_VERSION") v)
 	v)))

;; ----------------------------------------------------------------------------
;; Define the scheduler and others...

(defvar *scheduler* (make-instance 'scheduler:in-memory-scheduler))
(defvar *alphavantage-api-key* nil)
(defvar *equities* nil)
(defvar *funds* nil)
(defvar *fiats* nil)
(defvar *commodities* nil)
(defvar *repo-git-uri* nil)

;; ----------------------------------------------------------------------------
;; Find the directory in which we are installed.  This is used to
;; serve up static content.

(defun daily-price-depot-droid-root ()
  (fad:pathname-as-directory
   (make-pathname :name nil
                  :type nil
                  :defaults #.(or *compile-file-truename* *load-truename*))))

;; ----------------------------------------------------------------------------
;; Machinery for managing the execution of the server.

(defvar *shutdown-cv* (bt:make-condition-variable))
(defvar *server-lock* (bt:make-lock))

;; ----------------------------------------------------------------------------
;; Default configuration.  Overridden by external config file.
;; Config files are required to be in TOML format.

(defvar *config* nil)
(defvar *default-config* nil)
(defparameter +default-config-text+
"server-uri = \"http://localhost:8080\"
")

;; ----------------------------------------------------------------------------
;; The URI of the server.  Define this in your config.ini files.  Use
;; this is you are generating responses that point back to this
;; application.

(defvar *server-uri* nil)

;; ----------------------------------------------------------------------------
;; Initialize prometheus values.

(defparameter *http-requests-counter* nil)
(defparameter *http-request-duration* nil)

(defun initialize-metrics ()
  (unless *daily-price-depot-droid-registry*
    (setf *daily-price-depot-droid-registry* (prom:make-registry))
    (let ((prom:*default-registry* *daily-price-depot-droid-registry*))
      (setf *http-requests-counter*
            (prom:make-counter :name "http_requests_total"
                               :help "Counts http request by type"
                               :labels '("method" "app")))
      (setf *http-request-duration*
	    (prom:make-histogram :name "http_request_duration_milliseconds"
                                 :help "HTTP requests duration[ms]"
                                 :labels '("method" "app")
                                 :buckets '(10 25 50 75 100 250 500 750 1000 1500 2000 3000)))
      #+sbcl
      (prom.sbcl:make-memory-collector)
      #+sbcl
      (prom.sbcl:make-threads-collector)
      (prom.process:make-process-collector))))

;; ----------------------------------------------------------------------------
;; API routes

(defparameter *daily-price-depot-droid-registry* nil)

;; Readiness probe.  Always ready by default, but this can be as
;; complex as required.
(easy-routes:defroute health ("/health") ()
  "ready")

(markup:deftag page-template (children &key title)
   <html>
     <head>
       <meta charset="utf-8" />
       <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no" />
       <title>,(progn title)</title>
       <link rel="stylesheet" href="css/daily-price-depot-droid.css" />
     </head>
     <body>
     ,@(progn children)
     </body>
   </html>)

;; Render the home page.
(easy-routes:defroute index ("/") ()
  (markup:write-html
   <page-template title="daily-price-depot-droid">
   This is the index page of my new app, version ,(progn +daily-price-depot-droid-version+).
   </page-template>))

;; ----------------------------------------------------------------------------
;; HTTP server control

(defparameter *handler* nil)

(defparameter +daily-price-depot-droid-dispatch-table+
  (list
   (hunchentoot:create-folder-dispatcher-and-handler
    "/images/" (fad:pathname-as-directory
                (make-pathname :name "static/images"
                               :defaults (daily-price-depot-droid-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/js/" (fad:pathname-as-directory
            (make-pathname :name "static/js"
                           :defaults (daily-price-depot-droid-root))))
   (hunchentoot:create-folder-dispatcher-and-handler
    "/css/" (fad:pathname-as-directory
             (make-pathname :name "static/css"
                            :defaults (daily-price-depot-droid-root))))))

(defclass exposer-acceptor (prom.tbnl:exposer hunchentoot:acceptor)
  ())

(defclass application (easy-routes:easy-routes-acceptor)
  ((exposer :initarg :exposer :reader application-metrics-exposer)
   (mute-access-logs :initform t :initarg :mute-access-logs :reader mute-access-logs)
   (mute-messages-logs :initform t :initarg :mute-error-logs :reader mute-messages-logs)))

(defmacro stop-server (&key (handler '*handler*))
  "Shutdown the HTTP handler"
  `(hunchentoot:stop ,handler))

(defun start-server (&optional (config-ini "/etc/daily-price-depot-droid/config.ini"))

  (bt:with-lock-held (*server-lock*)

    (setf hunchentoot:*catch-errors-p* t)
    (setf hunchentoot:*show-lisp-errors-p* t)
    (setf hunchentoot:*show-lisp-backtraces-p* t)

    (log:info "Starting daily-price-depot-droid version ~A" +daily-price-depot-droid-version+)

    ;; Read the built-in configuration settings.
    (setf *default-config* (cl-toml:parse +default-config-text+))

    ;; Read the user configuration settings.
    (setf *config*
  	  (if (fad:file-exists-p config-ini)
	      (cl-toml:parse
	       (alexandria:read-file-into-string config-ini
					         :external-format :latin-1))
	      (make-hash-table)))

    (flet ((get-config-value (key)
	     (let ((value (or (gethash key *config*)
			      (gethash key *default-config*)
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
      (setf *equities* (get-config-value "equities"))
      (setf *funds* (get-config-value "funds"))
      (setf *fiats* (get-config-value "fiats"))
      (setf *commodities* (get-config-value "commodities"))
      (setf *repo-git-uri* (get-config-value "repo-git-uri"))

      (pull-repo (format nil "~A/daily-price-depot" (uiop:getenv "HOME"))
                 *repo-git-uri*)

      ;; Initialize prometheus
      (initialize-metrics)

      (log:info "Starting server")

      (scheduler:create-scheduler-task
       *scheduler*
       (format nil "~A (daily-price-depot-droid:pull-daily)" (get-config-value "cron-schedule")))

      ;; Start the scheduler
      (bt:make-thread (lambda () (scheduler:start-scheduler *scheduler*)))

      ;; (pull-daily)

      (setf hunchentoot:*dispatch-table* +daily-price-depot-droid-dispatch-table+)
      (setf prom:*default-registry* *daily-price-depot-droid-registry*)
      (setf *print-pretty* nil)
      (setf *handler* (let ((exposer (make-instance 'exposer-acceptor :registry *daily-price-depot-droid-registry* :port 9101)))
                        (hunchentoot:start (make-instance 'application
                                                          :document-root #p"./"
                                                          :port 8080
                                                          :exposer exposer))))

      (bt:condition-wait *shutdown-cv* *server-lock*))))

(defmethod hunchentoot:start ((app application))
  (hunchentoot:start (application-metrics-exposer app))
  (call-next-method))

(defmethod hunchentoot:stop ((app application) &key soft)
  (call-next-method)
  (hunchentoot:stop (application-metrics-exposer app) :soft soft))

(defmethod hunchentoot:acceptor-dispatch-request ((app application) request)
  (let ((labels (list (string-downcase (string (hunchentoot:request-method request)))
		      "daily-price-depot-droid_app")))
    (log:info *http-requests-counter*)
    (prom:counter.inc *http-requests-counter* :labels labels)
    (prom:histogram.time
     (prom:get-metric *http-request-duration* labels)
     (call-next-method))))
