;;; init.el --- Tomas Zellerins init file            -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tomas Zellerin

;; Author: Tomas Zellerin <tomas@zellerin.cz>
;; Keywords: local

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

;; Personal settings and preferences
(tool-bar-mode -1)
(menu-bar-mode -1)

(put 'narrow-to-region 'disabled nil) ; calling enable-command changes init file
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(set-input-method "czech-qwerty")

;; Packages
(load-library "package")
(package-initialize)

(setq package-archives
	     '(("gnu" . "https://elpa.gnu.org/packages/")
	       ("melpa" . "https://melpa.milkbox.net/packages/")))

(load-library "use-package")

;; Basic Lisp editing
(dolist (fn  '(delete-trailing-whitespace paredit-mode show-paren-mode))
  (add-hook 'lisp-mode-hook fn)
  (add-hook 'emacs-lisp-mode-hook fn))


;; Org mode is factored out
(use-package org
  :bind
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c r" . org-capture)
  :config
  (require 'tz-org-init))

;; Additional sources
(add-to-list 'load-path (concat  user-emacs-directory "lisp/"))
(setq custom-file "~/.emacs.d/lisp/custom.el")

(use-package message
  :defer t
  :config
  (require 'tz-mail))

(use-package gnus
  :bind ("<XF86Mail>" . gnus)
  :config
  (require 'tz-mail))

(load "experimental")
(load "tz-local")


;;; init.el ends here
