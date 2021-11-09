;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: DAILY-PRICE-DEPOT-DROID; Base: 10 -*-

#|

Copyright (C) 2021  Anthony Green <anthony@atgreen.org>

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public
License along with this program.  If not, see
<http://www.gnu.org/licenses/>.

|#

(in-package :daily-price-depot-droid)

(defun pull-repo (repo-dirname repo-git-uri)
  "Download the very latest version of REPO-GIT-URI into REPO-DIRNAME
from a git repo."
  (log:info "pull repo ~A" repo-git-uri)
  (let ((command (if (fad:directory-exists-p repo-dirname)
                     (format nil "bash -c \"(cd ~A; /usr/bin/git pull)\""
                             repo-dirname)
                     (format nil "GIT_TERMINAL_PROMPT=0 /usr/bin/git clone --depth 1 ~A ~A"
                             repo-git-uri repo-dirname))))
    (dolist (line (inferior-shell:run command))
      (log:info line))))

(defun commit-and-push-repo (repo-dirname)
  "Commit all changes in REPO-DIRNAME and push upstream."
  (log:info "commit and push repo ~A" repo-git-uri)
  (dolist (line (inferior-shell:run
                 (format nil "bash -c \"(cd ~A; /usr/bin/git add *)\""
                         repo-dirname)))
    (log:info line))
  (dolist (line (inferior-shell:run
                 (format nil "bash -c \"(cd ~A; /usr/bin/git commit -m 'Daily update')\""
                         repo-dirname)))
    (log:info line))
  (dolist (line (inferior-shell:run
                 (format nil "bash -c \"(cd ~A; /usr/bin/git push)\""
                         repo-dirname)))
    (log:info line)))
