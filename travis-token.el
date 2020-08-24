;;; travis-token.el ---                              -*- lexical-binding: t; -*-

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

(defvar travis-token nil)

(defun travis-set-token ()
  "Interactively set travis secret token."
  (interactive)
  (setq travis-token (read-string "Insert travis token: "))
  (travis-set-headers travis-token))

(defun travis-show-token ()
  "Interactively show Travis token."
  (interactive)
  (message "Travis token: %s" travis-token))

(provide 'travis-token)
;;; travis-token.el ends here
