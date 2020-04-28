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



(defun travis-show-builds-for-repo ()
  "Show builds for specified repo."
  (interactive)
  (request
    (travis-url-builds-for-repo (ido-completing-read
				 "Repositories: "
				 travis-bookmarked-repos))
    :type "GET"
    :headers travis-headers
    :parser (lambda ()
              (let ((json-array-type 'list))
		(json-read)))
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(travis-show-buffer-with-data "*BUILDS*"
					      (mapconcat 'travis-build-to-string
							 (assoc-default 'builds data)
							 "\n\n"))))))

(defun travis-build-to-string (build)
  "Return string to insert into display buffer for BUILD."
  (concat (format "Build for repository: %s\n" (assoc-default 'name (assoc-default 'repository build)))
	  (format "Build for branch: %s\n" (assoc-default 'name (assoc-default 'branch build)))
	  (format "Build for commit: %s\n" (assoc-default 'id (assoc-default 'commit build)))
	  (format "Build ID: %s\n" (assoc-default 'id build))
	  (format "Build number: %s\n" (assoc-default 'number build))
	  (format "Build state: %s\n" (assoc-default 'state build))
	  (format "Build previous state: %s\n" (assoc-default 'previous_state build))
	  (format "Build started at: %s\n" (assoc-default 'started_at build))
	  (format "Build finished at: %s\n" (assoc-default 'finished_at build))
	  (format "Build took: %s\n" (format-seconds
				      "%H %M %S"
				      (assoc-default 'duration build)))))

(provide 'travis-builds)
;;; travis-builds.el ends here
