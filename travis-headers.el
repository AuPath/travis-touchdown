;;; travis-headers.el ---                            -*- lexical-binding: t; -*-

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

(defvar travis-headers nil)
(defvar travis-headers-log nil)

(defun travis-set-headers (token)
  "Set token for http requests with security TOKEN."
  (setq travis-headers (backquote (("Travis-API-Version" . "3")
				   ("User-Agent" . "API Explorer")
				   ("Authorization" . ,(format "token %s" token)))))
  (setq travis-headers-log (append travis-headers '(("Accept" . "text/plain")))))



(provide 'travis-headers)
;;; travis-headers.el ends here
