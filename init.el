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
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-language-environment "UTF-8")
(set-face-attribute 'default nil :height 140)

;; calling enable-command changes init file
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; I need to write Czech easily.
(set-input-method "czech-qwerty")

;;; Packages setup
(require 'package)
(package-initialize)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")))

(unless (require 'use-package nil t)
  (package-initialize)
  (package-refresh-contents) ; this will be longer than usual
  (setq package-selected-packages nil)
  (package-install 'use-package)
  ;; Install from scratch: also, org should be reinstalled
  (package-install-from-archive (cadr (assoc 'org package-archive-contents)))
  (package-install-file "~/.emacs.d/lisp/tz-org-init.el")
  (package-install-file "~/.emacs.d/lisp/logical-pathnames.el"))

(setq use-package-always-ensure t)
;; This package needs to be called as early as possible so that the
;; other packages use correct files.
(use-package no-littering)


;; Basic Lisp editing
(use-package paredit
  :commands paredit-mode)

(dolist (fn '(paredit-mode show-paren-mode))
  (add-hook 'lisp-mode-hook fn)
  (add-hook 'emacs-lisp-mode-hook fn))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c r" . org-capture)
   ("C-c b" . org-iswitchb)))

(use-package tz-org-init
  :ensure nil
  :after org)

(use-package gnus
  :bind ("<XF86Mail>" . gnus)
  :config
  (setq gnus-select-method '(nnml "")
	gnus-use-adaptive-scoring '(word line)
	gnus-article-mime-part-function 'tz-mail-handle-attachment)

  (defun tz-mail-handle-attachment (handle)
    "Treat specifically attachments during mail opening."
    (cond
     ((equal (car (mm-handle-type handle)) "application/octet-stream")
      (save-excursion
	(with-temp-buffer
	  (insert (mm-get-part handle))
	  (goto-char 1)
	  (search-forward "windows-1250")
	  (recode-region (point-min) (point-max)
			 'windows-1250 'utf-8-unix )
	  (replace-match "utf-8")
	  (write-region (point-min) (point-max)
			(read-file-name "Save data to: " "~/ucty/"
					nil nil
					(mm-handle-filename handle)))))))))

(use-package message
  :ensure nil
  :commands message-mail message-news message-reply message-wide-reply
  message-forward
  :config
  (load "private" t)
  (setq message-send-mail-function 'smtpmail-send-it))

(use-package smtpmail
  :commands smtpmail-send-it
  :config
  (setq
   smtpmail-smtp-server "smtp.zoho.com"
   smtpmail-smtp-service 587))

(use-package nnmail
  :defer t :ensure nil
  :config
  (setq nnmail-split-methods
	'(("csob" "^From: .*\\(CSOB Administrator\\|tbs\\.csob\\.cz\\)")
	  ("EE World" "^From: .*\\(EE World\\|Design World\\)")
	  ("Conrad" "^From: .*Conrad Electronic")
	  ("mail.misc" ""))))

(use-package magit
  :commands magit-status magit magit-init magit-clone
  :bind (("C-c m" . magit)))

;; Additional sources
(add-to-list 'load-path (locate-user-emacs-file "lisp/"))
(setq custom-file (locate-user-emacs-file "local/custom.el"))

(use-package "nameless"
  :commands (nameless-mode))

(add-hook 'emacs-lisp-mode-hook
		     'nameless-mode)

(use-package "recentf"
  :config (setq recentf-exclude  '("emacs.d/elpa/" "/emacs/[0-9.]*/lisp/")))

(use-package "sly"
 :commands (sly sly-mode)
 :config
 (setq sly-lisp-implementations nil
       sly-net-coding-system 'utf-8-unix))

(use-package "eww"
  :defer t
  :config
  (defun my/eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page from cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

  (define-key eww-mode-map (kbd "I") #'my/eww-toggle-images)
  (define-key eww-link-keymap (kbd "I") #'my/eww-toggle-images)

  (setq-default shr-inhibit-images t)
  ; do something to shr-blocked-images?
  )

(bind-key (kbd "<f12> f") 'workflow-project-setup-frame)
(bind-key (kbd "<f12> RET") 'make-frame)

(bind-key (kbd "C-c c") 'compile)

(use-package "bbdb"
  :bind ("C-c B" . bbdb)
  :config
  (defun bbdb-swap-names ()
    (interactive)
    "Swap first and second name"
    (let ((first (bbdb-record-firstname (bbdb-current-record)))
	  (last (bbdb-record-lastname (bbdb-current-record))))
      (bbdb-record-set-field (bbdb-current-record) 'lastname first)
      (bbdb-record-set-field (bbdb-current-record) 'firstname last)
      (bbdb-redisplay-record (bbdb-current-record) t)
      (message "%s is surname now" first))))

(auto-insert-mode)
(push '(("\\.asm\\'" . "PIC midrange assembler") . pic-asm-new-file)
      auto-insert-alist)


(load custom-file t)
(load "experimental")
(load (locate-user-emacs-file "local/tz-local.el") t)



;;; init.el ends here
