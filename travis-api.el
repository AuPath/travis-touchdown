;;; travis-api.el --- Interface to Travis CI API     -*- lexical-binding: t; -*-

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

;; 

;;; Code:

;;; token : H60yeYCilDQ-htqJqjYHpw

;; search build by commit

(require 'request)
(require 'request-deferred)
(require 'json)

(defconst travis-api-url "https://api.travis-ci.com")

(defvar travis-headers nil)
(defvar travis-user-login nil)

(defun travis-set-user ()
  (interactive)
  (setq travis-user-login (read-string "User login: ")))

(defun travis-show-user ()
  (interactive)
  (message "User: %s" travis-user-login))

(defun travis-set-headers (token)
  (setq travis-headers (backquote (("Travis-API-Version" . "3")
			   ("User-Agent" . "API Explorer")
			   ("Authorization" . ,(format "token %s" token))))))
			    
travis-headers
  
(defconst travis-json-parser-options
  (lambda ()
    (let ((json-object-type 'hash-table)
	  (json-array-type 'list)
	  (json-key-type 'string))
      (json-read))))

(defvar travis-token nil)

(defun travis-set-token ()
  (interactive)
  (setq travis-token (read-string "Insert travis token: "))
  (travis-set-headers travis-token))

(defun travis-show-token ()
  (interactive)
  (message "Travis token: %s" travis-token))

travis-token

(defvar travis-owned-repos '())

(defvar travis-user-organizations '())

(defun travis-show-repos-owned ()
  (interactive)
  (message "Owned repositories: %s" travis-owned-repos))

(defun travis-show-organizations ()
  (interactive)
  (message "Organizations: %s" travis-user-organizations))

(defun travis-url-to-builds (project-name)
  (format "%s/repo/%s/builds" travis-api-url (url-hexify-string project-name)))

(defun travis-url-to-owned-repos (user-login)
  (format "%s/owner/%s/repos" travis-api-url user-login))

(defun travis-get-request (url buf buf-name stri get-data)
  (request
    url
    :type "GET"
    :headers travis-headers
    :parser travis-json-parser-options
    :error (message "error during get request")
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(funcall buf buf-name
			 (funcall stri (funcall get-data data)))))))

(defun travis-show-buffer-with-data (buffer-name data)
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert data)
    (pop-to-buffer (current-buffer))
    (goto-char (point-min))))

(defun travis-build-to-string (build)
  "Returns a string containing all relevant information from BUILD"
  (let ((build-status (gethash "state" build)))
    (cond ((equal "created" build-status)
	   (format "Repository: %s\nBranch: %s\nBuild ID: %s\nState: %s\n"
		   (gethash "name" (gethash "repository" build))
		   (gethash "name" (gethash "branch" build))
		   (gethash "id" build)
		   (gethash "state" build)))
	  ((equal "started" build-status)
	   (format "Repository: %s\nBranch: %s\nBuild ID: %s\nState: %s\nStarted: %s\n"
		   (gethash "name" (gethash "repository" build))
		   (gethash "name" (gethash "branch" build))
		   (gethash "id" build)
		   (gethash "state" build)
		   (gethash "started_at" build)))
	  (t
	   (format "Repository: %s\nBranch: %s\nBuild ID: %s\nState: %s\nStarted: %s\nFinished: %s\nDuration: %s"
		   (gethash "name" (gethash "repository" build))
		   (gethash "name" (gethash "branch" build))
		   (gethash "id" build)
		   (gethash "state" build)
		   (gethash "started_at" build)
		   (gethash "finished_at" build)
		   (format-seconds "%H %M %S" (gethash "duration" build)))))))

(defun travis-get-latest-build-for-repo ()
  (interactive)
  (travis-refresh-data)
  (travis-get-request (travis-url-to-builds (ido-completing-read "Available repositories: " travis-owned-repos))
		      'travis-show-buffer-with-data
		      "*Travis-builds*"
		      'travis-build-to-string
		      (lambda (x) (first (gethash "builds" x)))))

(defun travis-get-builds-for-repo ()
  (interactive)
  (travis-refresh-data)
  (travis-get-request (travis-url-to-builds (ido-completing-read "Available repositories: " travis-owned-repos))
		      'travis-show-buffer-with-data
		      "*Travis-builds*"
		      (lambda (x) (mapconcat 'travis-build-to-string x "\n\n"))
		      (lambda (x) (gethash "builds" x))))

(defun travis-get-active-repositories (user-login)
  (request
    (travis-url-to-owned-repos user-login)
    :type "GET"
    :headers travis-headers
    :parser travis-json-parser-options
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(setq travis-owned-repos (mapcar (lambda (x) (list (gethash "slug" x) (gethash "id" x)))
						 (gethash "repositories" data)))))))

(travis-get-active-repositories "AuPath")
(setq travis-owned-repos nil)

travis-owned-repos

(second (assoc "AuPath/ProvaJavaProgetto" travis-owned-repos))

(defun travis-refresh-data ()
  (interactive)
  (travis-get-active-repositories travis-user-login))

(defun travis-restart-build ()
  (interactive)
  (request
    (format "%s/build/%s/restart" travis-api-url (read-string "Build ID: "))
    :type "POST"
    :headers travis-headers
    :success (message "restarted build successfully")))

(defun travis-cancel-build ()
  (interactive)
  (request
    (format "%s/build/%s/cancel" travis-api-url (read-string "Build ID: "))
    :type "POST"
    :headers travis-headers
    :success (message "cancelled builds successfully")))

(defun travis-get-organizations ()
  (request
    (format "%s/orgs" travis-api-url)
    :type "GET"
    :headers travis-headers
    :parser travis-json-parser-options
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(setq travis-user-organizations
		      (mapcar (lambda (x) (gethash "login" x))
			      (gethash "organizations" data)))))))


(defun test-deferred ()
  
  (deferred:$
    (deferred:next
      (lambda () (travis-refresh-data)))
      (deferred:nextc it
	(lambda () (travis-get-latest-build-for-repo)))))

(test-deferred)

(deferred:$
    (request-deferred "http://httpbin.org/get" :parser 'json-read)
    (deferred:nextc it
      (lambda (response)
        (message "Got: %S" (request-response-data response)))))

(provide 'travis-api)
;;; travis-api.el ends here
