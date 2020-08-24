;;; travis-buffer.el ---                             -*- lexical-binding: t; -*-

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

(provide 'travis-buffer)
;;; travis-buffer.el ends here
