;;; tz-org-init.el --- Org mode preferences          -*- lexical-binding: t; -*-

;; Copyright (C) 2017, 2018  Tomas Zellerin

;; Author: Tomas Zellerin <tomas@zellerin.cz>
;; Version: 1.0
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:
(eval-after-load "ob"
  '(org-babel-lob-ingest "../emacs-setup.org"))

(org-babel-do-load-languages 'foo nil)

(add-hook 'org-mobile-post-push-hook 'tz-org-mobile-post-push)
(add-hook 'org-mobile-pre-pull-hook 'tz-org-mobile-pre-pull)

(require 'org-docview)

(provide 'tz-org-init)
;;; tz-org-init.el ends here
