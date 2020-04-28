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

(add-to-list 'load-path "~/Projects/Emacs-test")

(require 'request)
(require 'json)
(require 'travis-urls)
(require 'travis-bookmarks)
(require 'travis-urls)
(require 'travis-users)
(require 'travis-token)
(require 'travis-headers)
(require 'travis-builds)
(require 'travis-test-config)
(require 'travis-buffer)
(require 'travis-helper)

(provide 'travis-api)
;;; travis-api.el ends here
