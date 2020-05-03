;;; travis-builds.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Aurelio

;; Author: Aurelio <aurelio@aurelio-pc>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun travis-show-active-builds ()
  "Show active builds in a buffer."
  (interactive)
  (let ((chosen-user (ido-completing-read "User: " travis-user-list)))
    (travis-show-data "*ACTIVE-BUILDS*"
		      'travis-active-build-to-string
		      (travis-active-repos chosen-user))))

(defun travis-restart-build ()
  "Restart a build."
  (interactive)
  (travis-generic-request "POST"
			  (travis-url-build-restart
			   (read-string "Build id: " (thing-at-point 'word)))))

;; (defun travis-restart-build ()
;;   "Restart a build."
;;   (interactive)
;;   (request
;;     (travis-url-build-restart (read-string "Build id: " (thing-at-point 'word)))
;;     :type "POST"
;;     :headers travis-headers
;;     :parser 'json-read
;;     :sync t
;;     :success (cl-function
;; 	      (lambda (&key data &allow-other-keys)
;; 		(message "Successfuly restarted build")))))

(defun travis-show-builds-for-repo()
    "Show builds for specified repo."
  (interactive)
  (let ((chosen-repo (ido-completing-read
		      "Repositories: "
		      travis-bookmarked-repos)))
    (travis-show-data (format "*BUILDS[%s]*" chosen-repo)
		      'travis-build-to-string
		      (assoc-default 'builds
				     (travis-generic-request "GET"
							     (travis-url-builds-for-repo chosen-repo))))))

(defun travis-build-to-string (build)
  "Return string to insert into display buffer for BUILD."
  (concat (format "Branch: %s\n" (assoc-default 'name (assoc-default 'branch build)))
	  (format "Commit: %s\n" (assoc-default 'id (assoc-default 'commit build)))
	  (format "Id: %s\n" (assoc-default 'id build))
	  (format "Number: %s\n" (assoc-default 'number build))
	  (format "State: %s\n" (assoc-default 'state build))
	  (format "Previous state: %s\n" (assoc-default 'previous_state build))
	  (format "Started at: %s\n" (assoc-default 'started_at build))
	  (format "Finished at: %s\n" (assoc-default 'finished_at build))
	  (format "Duration: %s\n" (format-seconds
				    "%H %M %S"
				    (assoc-default 'duration build)))))

(provide 'travis-builds)
;;; travis-builds.el ends here
