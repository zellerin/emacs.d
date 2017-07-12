;;; tz-org-init.el --- Org mode preferences          -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tomas Zellerin

;; Author: Tomas Zellerin <tomas@zellerin.cz>
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


(setq org-modules '(org-bbdb org-gnus)
      org-hide-emphasis-markers t
      org-id-link-to-org-use-id t
      org-directory "~/org"
      org-log-into-drawer t)

(defun tz-export-subtree ()
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (let ((file (org-id-get-create))
	  (has-name (org-entry-get (point) "EXPORT_FILE_NAME")))
      (unless has-name
	(org-entry-put (point) "EXPORT_FILE_NAME" file))
      (save-excursion
	(org-export-as-html 4 nil nil nil nil "/tmp/"))
      (unless has-name (undo-only)))))

(setq org-refile-use-outline-path nil)

(eval-after-load "ob"
  '(setq org-confirm-babel-evaluate nil))

(eval-after-load "org-src"
  '(progn
     (push '("dot" . graphviz-dot) org-src-lang-modes)))

(push '(dot . t) org-babel-load-languages)
(push '(lisp . t) org-babel-load-languages)
(org-babel-do-load-languages 'foo nil)

(eval-after-load org-agenda
  '(setq org-agenda-files `(,org-directory)
	org-agenda-todo-ignore-scheduled 'future
	org-scheduled-past-days 1
	org-agenda-custom-commands nil))

(use-package org-capture
  :config
  (setq org-capture-templates
	'(
	  ("j" "Weekly entry" entry
	     (file+headline "~/org/journal.org" "2017")
	     "* Week %(format-time-string \"\\%U\")" :prepend t)
	  ("t" "TODO" entry
	   (file "weekly-review.org")
	   "* 🔨 %?\n%T" :prepend t :clock-in t :clock-resume t)
	  ("m" "Flag mail" entry (file "weekly-review.org") "* %:subject\n%a")
	  ("f" "Flag place" entry (file "weekly-review.org") "* %?\n%T\n%A")
	  ("r" "Remind person" entry (file "weekly-review.org") "* %:name\n%a")
	  ("w" "Remind web" entry (file "weekly-review.org") "* %(save-excursion (set-buffer (get-buffer \"*eww*\")) (plist-get eww-data :title))\n%T\n%(save-excursion (set-buffer (get-buffer \"*eww*\")) (eww-current-url))"))
	org-capture-templates-contexts
	'(("f" "w" #1=((in-mode . "eww-mode")))
	  ("w" #1#)
	  ("f" "m" #2=((in-mode . "article-mode")))
	  ("m" #2#)
	  ("f" "r" #3=((in-mode . "bbdb-mode")))
	  ("r" #3#)
	  ("f" ((not-in-mode . "bbdb-mode"))))))

(use-package org-attach
  :config
  (setq org-attach-file-list-property nil
	org-attach-method 'mv))

(provide 'tz-org-init)
;;; tz-org-init.el ends here
