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

(asdf:defsystem #:daily-price-depot-droid
  :description "Update ledger-compatible price data in a git repo."
  :author "Anthony Green <anthony@atgreen.org>"
  :version "0"
  :serial t
  :components ((:file "package")
               (:file "fetch")
               (:file "pull-repo")
	       (:file "daily-price-depot-droid"))
  :depends-on (:alexandria
               :cl-csv
               :cl-date-time-parser
               :cl-fad
               :cl-json
               :cl-toml
               :drakma
               :easy-routes
               :flexi-streams
               :hunchentoot
               :inferior-shell
               :log4cl
               :markup
               :prometheus
               :prometheus.collectors.process
               :prometheus.collectors.sbcl
               :prometheus.exposers.hunchentoot
               :prometheus.formats.text
               :scheduler
               :split-sequence
               :str))
