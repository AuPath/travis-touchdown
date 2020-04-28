;;; travis-bookmarks.el --- Interface to Travis CI API     -*- lexical-binding: t; -*-

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


(defvar travis-bookmarked-repos nil)

(defun travis-show-bookmarked-repos ()
  "Show all bookmarked repos."
  (interactive)
  (message "Bookmarked repos: %s " travis-bookmarked-repos))

(defun travis-reset-bookmarked-repos ()
  "Set variable TRAVIS-BOOKMARKED-REPOS to nil."
  (interactive)
  (setq travis-bookmarked-repos nil))

(defun travis-add-repo-to-bookmarks ()
  "Add repo to TRAVIS-BOOKMARKED-REPOS."
  (interactive)
  (add-to-list 'travis-bookmarked-repos (completing-read
					 "Repos: "
					 (travis-request-user-repos
					  (completing-read
					   "Users: "
					   travis-user-list)))))

(defun travis-delete-repo-from-bookmarks-helper (repo-slug)
  "Delete REPO-SLUG from TRAVIS-BOOKMARKED-REPOS."
  (setq travis-bookmarked-repos (delete repo-slug travis-bookmarked-repos)))

(defun travis-delete-repo-from-bookmarks ()
  (interactive)
  (travis-delete-repo-from-bookmarks-helper (completing-read "Repo: " travis-bookmarked-repos)))

(provide 'travis-bookmarks)
;;; travis-bookmarks.el ends here

