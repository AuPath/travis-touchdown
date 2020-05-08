;;; travis-faces.el ---                              -*- lexical-binding: t; -*-

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

(defface travis-builds-keyword '((t :inherit font-lock-builtin-face
				    :weight bold))
  "Face for highlighting travis build keywords.")

(defface travis-failed-build '((t :inherit error))
  "Face for highlighting a failed travis build.")

(defface travis-warning-build '((t :inherit warning))
  "Face for highlighting a warning travis build.")

(defface travis-successful-build '((t :inherit success))
  "Face for highlighting a successful travis build.")

(defvar travis-symbols-build '("Repository" "Branch" "Commit" "Id" "Number" "State" "Previous state" "Started at" "Finished at" "Duration" "Message"))


(defun travis-set-faces-builds ()
  "Set faces for travis build buffer."
  (font-lock-add-keywords nil '(
				("\\(\\(?:Branch\\|Commit\\|Duration\\|Finished at\\|Id\\|Message\\|Number\\|Previous state\\|Repository\\|Sta\\(?:rted at\\|te\\)\\):\\)" . 'travis-builds-keyword)
				("\\(passed\\)" . 'travis-successful-build)
				("\\(failed\\)" . 'travis-failed-build)
				("\\(\\(?:c\\(?:ancel\\|reat\\)\\|restart\\)ed\\)" . 'travis-warning-build))))

(regexp-opt '("canceled" "restarted" "created") t)

(regexp-opt '("passed") t)

(regexp-opt (mapcar (lambda (x) (format "%s:" x)) travis-symbols-build) t)

(provide 'travis-faces)
;;; travis-faces.el ends here
