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

;;; Personal settings and preferences for core functions
;; I do not need tool bar and status bar
(tool-bar-mode -1) ; -> custom.el
(menu-bar-mode -1)
(set-language-environment "UTF-8") ; customize current-language-environment

;; calling enable-command changes init file
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;; Packages setup
(package-initialize)

;; This package needs to be called as early as possible so that the
;; other packages use correct files.
(require 'no-littering)

(dolist (fn '(paredit-mode show-paren-mode))
  (add-hook 'lisp-mode-hook fn)
  (add-hook 'emacs-lisp-mode-hook fn))

(add-hook 'emacs-lisp-mode-hook 'nameless-mode)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(bind-key "C-c a" 'org-agenda)
(bind-key "C-c b" 'org-switchb)
(bind-key "C-c c" 'compile)
(bind-key "C-c l" 'org-store-link)
(bind-key "C-c m" 'magit-status)
(bind-key "C-c r" 'org-capture)
(bind-key "C-c \"" 'poporg-dwim)
(with-eval-after-load 'conf-mode
  (define-key conf-mode-map "\C-c\"" nil))

;; my custom.el should take preference to default custom.el
(defun customize-standard-value-of (symbol)
  (eval (car (get
	      symbol
	      'standard-value))))

(load (setq custom-file (locate-user-emacs-file "local/custom.el")) t)

(require 'autoload) ; otherwise binding below does not work

(defcustom tz-autoload-directories
  '("~/.emacs.d/autoloaded.d/"
    "~/.emacs.d/local/"
    "~/.emacs.d/linux/")
  "List of directories to autoload"
  :group 'tz)

(let ((generated-autoload-file "~/.emacs.d/autoloaded.d/autoloads.el"))
  (when t
    (apply #'update-directory-autoloads tz-autoload-directories))
  (load generated-autoload-file t))

(add-to-list 'load-path (locate-user-emacs-file "autoloaded.d/"))

(defcustom tz-mail-template
  "* %:subject\n%a\n"
  "Mail capture template.")

(load (locate-user-emacs-file "local/tz-local.el") t)
(load-theme 'tz)

;;; init.el ends here
