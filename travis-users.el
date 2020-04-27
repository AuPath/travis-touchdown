;;; travis-users.el --- Interface to Travis CI API     -*- lexical-binding: t; -*-

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

(defvar travis-user-login nil)
(defvar travis-user-list nil)

(defun travis-reset-user-list ()
  "Set variable TRAVIS-USER-LIST to nil"
  (interactive)
  (setq travis-user-list nil))

(defun travis-refresh-user-list ()
  "Add orgs to user list."
  (mapc (lambda (x) (add-to-list 'travis-user-list x)) (travis-request-user-orgs)))

(defun travis-add-user ()
  "Add USER to list of known users TRAVIS-USER-LIST."
  (interactive)
  (add-to-list 'travis-user-list (read-string "User login: ")))

(defun travis-delete-repo-from-bookmarks (repo-slug)
  "Delete REPO-SLUG from TRAVIS-BOOKMARKED-REPOS."
  (setq travis-bookmarked-repos (delete repo-slug travis-bookmarked-repos)))

(defun travis-set-user ()
  "Interactively set Travis's username to be used in requests."
  (interactive)
  (setq travis-user-login (read-string "User login: ")))

(defun travis-show-user ()
  "Interactively show Travis username."
  (interactive)
  (message "User: %s" travis-user-login))


(provide 'travis-users)
;;; travis-users.el ends here