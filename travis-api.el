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

;;; Code:

(add-to-list 'load-path "~/Projects/travis-elisp")

(require 'request)
(require 'json)
(require 'travis-test-config)

(defgroup travis nil
  "Travis interface for emacs."
  :prefix "travis-"
  :group 'tools)

(define-minor-mode travis-mode
  "Travis."
  :lighter "Trv")

(define-derived-mode travis-builds-mode special-mode "Travis[Builds]"
  "Major mode for displaying build data from Travis CI"
  (travis-set-faces-builds))

(defvar travis-bookmarked-repos nil)

(defface travis-builds-keyword '((t :inherit font-lock-builtin-face
				    :weight bold)
				 :group 'travis-builds-mode)
  "Face for highlighting travis build keywords.")

(defface travis-failed-build '((t :inherit error) :group 'travis-builds-mode)
  "Face for highlighting a failed travis build.")

(defface travis-warning-build '((t :inherit warning) :group 'travis-builds-mode)
  "Face for highlighting a warning travis build.")

(defface travis-successful-build '((t :inherit success) :group 'travis-builds-mode)
  "Face for highlighting a successful travis build.")

(defvar travis-symbols-build '("Repository" "Branch" "Commit" "Id" "Number" "State" "Previous state" "Started at" "Finished at" "Duration" "Message"))


(defun travis-set-faces-builds ()
  "Set faces for travis build buffer."
  (font-lock-add-keywords nil '(
				("\\(\\(?:Branch\\|Commit\\|Duration\\|Finished at\\|Id\\|Message\\|Number\\|Previous state\\|Repository\\|Sta\\(?:rted at\\|te\\)\\):\\)" . 'travis-builds-keyword)
				("\\(passed\\)" . 'travis-successful-build)
				("\\(failed\\)" . 'travis-failed-build)
				("\\(\\(?:c\\(?:ancel\\|reat\\)\\|restart\\)ed\\)" . 'travis-warning-build))))

(defvar travis-headers nil)
(defvar travis-headers-log nil)

(defun travis-set-headers (token)
  "Set token for http requests with security TOKEN."
  (setq travis-headers (backquote (("Travis-API-Version" . "3")
				   ("User-Agent" . "API Explorer")
				   ("Authorization" . ,(format "token %s" token)))))
  (setq travis-headers-log (append travis-headers '(("Accept" . "text/plain")))))

(defvar travis-token nil)

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
			   (read-string "Build id: " (thing-at-point 'word)))
			  travis-headers))

(defun travis-cancel-build ()
  "Cancel a build."
  (interactive)
  (travis-generic-request "POST"
			  (travis-url-build-cancel
			   (read-string "Build id: " (thing-at-point 'word)))
			  travis-headers))

(defun travis-show-builds-for-repo()
    "Show builds for specified repo."
  (interactive)
  (let ((chosen-repo (ido-completing-read
		      "Repositories: "
		      travis-bookmarked-repos)))
    (travis-show-data (format "*BUILDS[%s]*" chosen-repo)
		      'travis-build-to-string
		      (assoc-default 'builds
				     (travis-build-data chosen-repo)))))

(defun travis-build-data (repo-slug)
  "Get all build data for repo REPO-SLUG."
  (travis-generic-request "GET"
			  (travis-url-builds-for-repo repo-slug)
			  travis-headers))

;; 160097308
(defun travis-active-repos (user)
  "Return a-list with data on repositories owned by user that are being built."
  (assoc-default 'builds (travis-generic-request
			  "GET"
			  (travis-url-active-builds user)
			  travis-headers)))



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
				    (assoc-default 'duration build)))
	  (format "Message: %s\n" (assoc-default 'message
						 (assoc-default 'commit build)))))

(defun travis-active-build-to-string (active-build)
  "ACTIVE-BUILD to string."
  (concat (format "Repository: %s\n"
		  (assoc-default 'slug
				 (assoc-default 'repository active-build)))
	  (format "Branch: %s\n"
		  (assoc-default 'name
				 (assoc-default 'branch active-build)))
	  (format "Id: %s\n" (assoc-default 'id active-build))))



(defun travis-add-repo-to-bookmarks ()
  "Add repo to TRAVIS-BOOKMARKED-REPOS."
  (interactive)
  (add-to-list 'travis-bookmarked-repos (completing-read
					 "Repos: "
					 (travis-user-repos
					  (completing-read
					   "Users: "
					   travis-user-list)))))

(defun travis-delete-repo-from-bookmarks-helper (repo-slug)
  "Delete REPO-SLUG from TRAVIS-BOOKMARKED-REPOS."
  (setq travis-bookmarked-repos (delete repo-slug travis-bookmarked-repos)))

(defun travis-delete-repo-from-bookmarks ()
  (interactive)
  (travis-delete-repo-from-bookmarks-helper (completing-read "Repo: " travis-bookmarked-repos)))

(defun travis-show-buffer-with-data (buffer-name data)
  "Open a buffer named BUFFER-NAME and insert DATA."
  (when (get-buffer buffer-name)
    (kill-buffer buffer-name))
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert data)
    (travis-builds-mode)
    (pop-to-buffer (buffer-name))
    (goto-char (point-min))))

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



(defconst travis-url-api "https://api.travis-ci.com"
  "Url to base api for Travis")

(defconst travis-url-lint (concat travis-url-api "/lint")
  "Url to travis linter")

(defun travis-url-jobs-for-build (build-id)
    "Url to get jobs for build with BUILD-ID."
    (format "%s/build/%s/jobs" travis-url-api build-id))

(defun travis-url-owned-repos (user-login)
  "Return url to repositories owned by USER-LOGIN."
  (format "%s/owner/%s/repos" travis-url-api user-login))

(defun travis-url-job-log (job-id)
  "Return url to log on job with JOB-ID."
  (format "%s/job/%s/log" travis-url-api job-id))

(defun travis-url-builds-for-repo (repo-slug)
  "Return url for getting builds for repo with REPO-SLUG."
  (format "%s/repo/%s/builds" travis-url-api (url-hexify-string repo-slug)))

(defun travis-url-active-builds (user-login)
  "Return url for getting active builds for user USER-LOGIN."
  (format "%s/owner/%s/active" travis-url-api user-login))

(defvar travis-url-to-orgs (format "%s/orgs" travis-url-api))

(defun travis-url-build-restart (build-id)
  "Return url to restart build with BUILD-ID."
  (format "%s/build/%s/restart" travis-url-api build-id))

(defun travis-url-build-cancel (build-id)
  "Return url to cancel build with BUILD-ID."
  (format "%s/build/%s/cancel" travis-url-api build-id))

(defun travis-url-repo-branches (repo-slug)
  "Return url to get branches for REPO-SLUG."
  (format "%s/repo/%s/branches" travis-url-api (url-hexify-string repo-slug)))

(defvar travis-user-login nil)
(defvar travis-user-list nil)

(defun travis-reset-user-list ()
  "Set variable TRAVIS-USER-LIST to nil."
  (interactive)
  (setq travis-user-list nil))

(defun travis-refresh-user-list ()
  "Add orgs to user list."
  (add-to-list 'travis-user-list travis-user-login)
  (mapc (lambda (x) (add-to-list 'travis-user-list x)) (travis-orgs-for-user)))

(defun travis-set-user-login ()
  "Interactively set Travis's username to be used in requests."
  (interactive)
  (setq travis-user-login (read-string "User login: "))
  (travis-refresh-user-list))

(defun travis-show-user-login ()
  "Interactively show Travis username."
  (interactive)
  (message "User: %s" travis-user-login))

(projectile-project-root)

;; project-root . repo
(defvar project-repo-list '(("/home/aurelio/Projects/travis-elisp/"
			     . "AuPath/ProvaJavaProgetto")))

(assoc-default (projectile-project-root) project-repo-list)
 

(defun travis-associate-project-to-repo ()
  "Add project root to known repo."
  (add-to-list 'project-repo-list (list (projectile-project-root)
					(completing-read
					 "Repos: "
					 (travis-user-repos
					  (completing-read
					   "Users: "
					   travis-user-list))))))
(defun travis-lint-config-file ()
  "Lint config file."
  (interactive)
  (request
    travis-url-lint
    :type "POST"
    :files `(("yaml-file" . ,(current-buffer)))
    :parser 'json-read
    :success (cl-function
           (lambda (&key data &allow-other-keys)
             (message "I sent: %S" (assoc-default 'files data))))))

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

(provide 'travis-api)
;;; travis-api.el ends here
