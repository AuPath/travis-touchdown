;;; travis-jobs.el ---                               -*- lexical-binding: t; -*-

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

(defun travis-jobs-for-build (build-id)
  "Return all jobs for BUILD-ID at point."
  (travis-generic-request "GET" (travis-url-jobs-for-build
				 build-id)
			  travis-headers))

(defun travis-job-log ()
  "Return log for job with job id at point."
  (interactive)
  (assoc-default 'content
		 (travis-generic-request
		  "GET"
		  (travis-url-job-log
		   (read-string "Job id: " (thing-at-point 'word)))
		  travis-headers)))

;; (travis-show-buffer-with-data "Log" (replace-regexp-in-string "" "\n" (travis-job-log)))

(defun travis-job-to-string (job)
  "JOB to buffer."
  (concat
   (format " Number: %s\n" (assoc-default 'number job))
   (format " Id: %s\n" (assoc-default 'id job))
   (format " State: %s\n" (assoc-default 'state job))
   (format " Started at: %s\n" (assoc-default 'started_at job))
   (format " Finished at: %s\n" (assoc-default 'finished_at job))
   (format " Duration: %s\n" (format-seconds
			      "%H %M %S"
			      (assoc-default 'duration job)))))

(mapconcat 'travis-job-to-string
	   (assoc-default 'jobs (travis-jobs-for-build "160097308"))
	   "\n")




(provide 'travis-jobs)
;;; travis-jobs.el ends here
