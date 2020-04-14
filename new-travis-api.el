;;; new-travis-api.el --- Interface to Travis CI API     -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Aurelio

;; Author: Aurelio <aurelio@aurelio-pc>
;; Keywords: convenience

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

;;; Code:

;;; token : H60yeYCilDQ-htqJqjYHpw

(require 'request)
(require 'json)

;; API URLs
(defconst travis-api-url "https://api.travis-ci.com")
(defconst travis-lint-url (concat travis-api-url "/lint"))

(defun travis-url-owned-repos (user-login)
  "Return url to repositories owned by USER-LOGIN."
  (format "%s/owner/%s/repos" travis-api-url user-login))

(defun travis-url-builds-for-repo (repo-slug)
  "Return url for getting builds for repo with REPO-SLUG."
  (format "%s/repo/%s/builds" travis-api-url (url-hexify-string repo-slug)))

(defun travis-url-active-builds (user-login)
  "Return url for getting active builds for user USER-LOGIN."
  (format "%s/owner/%s/active" travis-api-url user-login))

(defvar travis-url-to-orgs (format "%s/orgs" travis-api-url))

(defun travis-url-repo-branches (repo-slug)
  "Return url to get branches for REPO-SLUG."
  (format "%s/repo/%s/branches" travis-api-url (url-hexify-string repo-slug)))

;; VARIABLES
(defvar travis-token nil)
(defvar travis-user-login nil)
(defvar travis-headers nil)

;; CONFIG FUNCTIONS

(defun travis-set-headers (token)
  "Set token for http requests with security TOKEN."
  (setq travis-headers (backquote (("Travis-API-Version" . "3")
			   ("User-Agent" . "API Explorer")
			   ("Authorization" . ,(format "token %s" token))))))

(defun travis-set-user ()
  "Interactively set Travis's username to be used in requests."
  (interactive)
  (setq travis-user-login (read-string "User login: ")))

(defun travis-show-user ()
  "Interactively show Travis username."
  (interactive)
  (message "User: %s" travis-user-login))

(defun travis-set-token ()
  "Interactively set travis secret token."
  (interactive)
  (setq travis-token (read-string "Insert travis token: "))
  (travis-set-headers travis-token))

(defun travis-show-token ()
  "Interactively show Travis token."
  (interactive)
  (message "Travis token: %s" travis-token))

;; REQUEST FUNCTIONS

(defun travis-show-builds-for-repo ()
  "Show builds for specified repo."
  (interactive)
  (request
    (travis-url-builds-for-repo (ido-completing-read
				 "Repositories: "
				 (travis-request-user-repos)))
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
;; HELPER FUNCTIONS

(defun travis-request-user-repos ()
  "Request user owned repos for user stored in owner-repos."
  (let ((response
  (request
    (travis-url-owned-repos travis-user-login)
    :type "GET"
    :headers travis-headers
    :parser 'json-read
    :sync t
    )))
    (mapcar (lambda (x) (assoc-default 'slug x)) (assoc-default 'repositories (request-response-data response)))))

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

(defun travis-show-buffer-with-data (buffer-name data)
  "Open a buffer named BUFFER-NAME and insert DATA."
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert data)
    (pop-to-buffer (current-buffer))
    (goto-char (point-min))))

;;; CONFIG ON EVAL

(setq travis-user-login "AuPath")
(setq travis-token "H60yeYCilDQ-htqJqjYHpw")
(travis-set-headers travis-token)

(provide 'new-travis-api)
;;; new-travis-api.el ends here
