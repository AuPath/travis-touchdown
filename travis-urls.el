;;; travis-urls.el --- Interface to Travis CI API     -*- lexical-binding: t; -*-

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

(defun travis-url-build-restart (build-id)
  "Return url to restart build with BUILD-ID."
  (format "%s/build/%s/restart" travis-api-url build-id))

(defun travis-url-build-cancel (build-id)
  "Return url to cancel build with BUILD-ID."
  (format "%s/build/%s/cancel" travis-api-url build-id))

(defun travis-url-repo-branches (repo-slug)
  "Return url to get branches for REPO-SLUG."
  (format "%s/repo/%s/branches" travis-api-url (url-hexify-string repo-slug)))

(provide 'travis-urls)
;;; travis-urls.el ends here
