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

(defun travis-url-owned-repos (user-login)
  "Return url to repositories owned by USER-LOGIN."
  (format "%s/owner/%s/repos" travis-api-url user-login))

(defun travis-url-builds-for-repo (repo-slug)
  "Return url for getting builds for REPO-SLUG."
  (format "%s/repo/%s/builds" travis-api-url (url-hexify-string repo-slug)))

;; VARIABLES
(defvar travis-token nil)
(defvar travis-user-login nil)
(defvar travis-user-repos-verbose nil)
(defvar travis-headers nil)

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

(defun travis-request-user-repos ()
  "Request user owned repos for user stored in owner-repos."
  (request
    (travis-url-owned-repos travis-user-login)
    :type "GET"
    :headers travis-headers
    :parser 'json-read
    :sync t
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(setq travis-user-repos-verbose (mapcar (lambda (x) (cons (assoc-default 'name x) (assoc-default 'slug x))) (assoc-default 'repositories data)))))))

(defun travis-request-builds-for-repo ()
  "Request builds for specified repo."
  (request
    (travis-url-builds-for-repo (travis-get-repo-slug-from-name
				 (ido-completing-read "Available repositories: " (travis-repos-name))))
    :type "GET"
    :header travis-headers
    :parser 'json-read
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(message "prova")))))

(travis-request-builds-for-repo)
(travis-request-user-repos)

(defun travis-repos-name ()
  "Return a list containg the name of the repos owned by the user."
  (mapcar 'first travis-user-repos-verbose))

(defun travis-get-repo-slug-from-name (repo-name)
  "Return repo slug for repo REPO-NAME."
  (assoc-default repo-name travis-user-repos-verbose))

(defun travis-show-buffer-with-data (buffer-name data)
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert data)
    (pop-to-buffer (current-buffer))
    (goto-char (point-min))))

(provide 'new-travis-api)
;;; new-travis-api.el ends here
