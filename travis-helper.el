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

(defun travis-request-user-repos (user)
  "Request user owned repos for user stored in owner-repos."
  (let ((response
	 (request
	   (travis-url-owned-repos user)
	   :type "GET"
	   :headers travis-headers
	   :parser 'json-read
	   :sync t
	   )))
    (mapcar (lambda (x) (assoc-default 'slug x))
	    (assoc-default 'repositories (request-response-data response)))))

(defun travis-request-user-repo-branches (repo-name)
  "Request user owned repo branches for REPO-NAME."
  (let ((response
  (request
    (travis-url-repo-branches repo-name)
    :type "GET"
    :headers travis-headers
    :parser 'json-read
    :sync t
    )))
    (request-response-data response)))

(defun travis-request-user-orgs ()
  "Return a list of the organizations the user belongs to."
  (let ((response
	 (request
	   travis-url-to-orgs
	   :type "GET"
	   :headers travis-headers
	   :parser 'json-read
	   :sync t
	   )))
    (mapcar (lambda (x) (assoc-default 'login x))
	    (assoc-default 'organizations (request-response-data response)))))

(provide 'travis-helper)
;;; travis-helper.el ends here
