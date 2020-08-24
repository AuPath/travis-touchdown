;;; travis-helper.el ---                             -*- lexical-binding: t; -*-

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

(defun travis-generic-request (method url headers)
  "Generic HTTP request."
  (let ((response
	 (request
	   url
	   :type method
	   :headers headers
	   :parser 'json-read
	   :sync t )))
    (request-response-data response)))

(defun travis-user-repos (user)
  "Return a list of repo-slugs of repos owned by USER."
  (mapcar (lambda (x) (assoc-default 'slug x))
	  (assoc-default 'repositories
			 (travis-generic-request "GET"
						 (travis-url-owned-repos user)
						 travis-headers))))

(defun travis-branches-for-repo (repo-slug)
  "Request user owned repo branches for REPO-SLUG."
  (mapcar (lambda (x) (assoc-default 'name x))
	  ( assoc-default 'branches
			  (travis-generic-request "GET"
						  (travis-url-repo-branches repo-slug)))))

(defun travis-orgs-for-user ()
  "Return a list of the organizations the user belongs to."
  (mapcar (lambda (x) (assoc-default 'login x))
	  (assoc-default 'organizations (travis-generic-request "GET"
								travis-url-to-orgs
								travis-headers))))

(defun travis-show-data (buf-name to-string data)
  "Open buffer with name BUF-NAME, where DATA is shown after being modeled by TO-STRING function name."
  (travis-show-buffer-with-data buf-name (mapconcat to-string data "\n\n")))

(provide 'travis-helper)
;;; travis-helper.el ends here
