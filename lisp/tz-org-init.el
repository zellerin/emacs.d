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
      org-log-into-drawer t
      org-refile-use-outline-path 'file
      org-archive-location (concat org-directory "/archive/2018.org::datetree/* Finished tasks")
      org-enforce-todo-dependencies t
      org-src-window-setup 'current-window
      org-agenda-dim-blocked-tasks 'invisible)

(setq org-refile-targets
      '((org-agenda-files :maxlevel . 2)
	(nil :maxlevel . 5)))

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

(eval-after-load "ob"
  '(setq org-confirm-babel-evaluate nil))

(eval-after-load "org-src"
  '(progn
     (push '("dot" . graphviz-dot) org-src-lang-modes)))

(setq org-babel-load-languages
      '((lisp . t)
	(dot . t)
	(emacs-lisp . t)
	(sh . t))
      org-babel-lisp-eval-fn (quote sly-eval))

(org-babel-do-load-languages 'foo nil)

(eval-after-load "org-agenda"
  '(progn
     (bind-key [32] 'org-agenda-show-narrowed
	       org-agenda-mode-map)

     (defun org-agenda-show-narrowed ()
       (interactive)
       (org-agenda-show-and-scroll-up)
       (let ((win (selected-window)))
	 (select-window org-agenda-show-window)
	 (org-narrow-to-subtree)
	 (select-window win)))

     ;;; Org agenda random variable setup
     (setq org-agenda-todo-ignore-scheduled 'future
	   org-scheduled-past-days 1
	   org-agenda-custom-commands nil)))

(eval-after-load "org-capture"
  '(setq org-capture-templates
      '(("j" "Weekly entry" entry
	 (file+headline "~/journal.org" "2018")
	 "* Week %(format-time-string \"\\%U\")\n" :prepend t)
	("t" "TODO" entry
	 (file "weekly-review.org")
	 "* TODO %?\n%U\n" :prepend t :clock-in t :clock-resume t)
	("m" "Flag mail" entry
	(file "weekly-review.org")
	"* %:subject\n%a\n")
	("f" "Flag place" entry
	 (file "weekly-review.org")
	 "* %?\n%T\n%A\n")
	("r" "Remind person" entry
	 (file "weekly-review.org")
	 "* %:name\n%a\n")
	("w" "Remind web" entry
	 (file "weekly-review.org")
	 "* %(tz-capture-from-eww)\n%T\n\n%(tz-eww-url)\n")
	("P" "Project subtask" entry
	 (file "weekly-review.org")
	 "* TODO %?\n  :PROPERTIES:\n    :CATEGORY: %(with-current-buffer (find-buffer-visiting \"%F\") (org-get-category))\n  :END:\n[[%l][Project info]]\n\n"))
      org-capture-templates-contexts
	'(("f" "w" #1=((in-mode . "eww-mode")))
	  ("w" #1#)
	  ("f" "m" #2=((in-mode . "article-mode")))
	  ("m" #2#)
	  ("f" "m" #2=((in-mode . "gnus-summary-mode")))
	  ("m" #2#)
	  ("f" "r" #3=((in-mode . "bbdb-mode")))
	  ("r" #3#)
	  ("f" ((not-in-mode . "bbdb-mode"))))))

(eval-after-load "org-attach"
  '(setq org-attach-file-list-property nil
	org-attach-method 'mv))

(defun org-attached-tag (a)
  "Returns path to file in attach directory. To be used in a link abbreviation."
  (concat (org-attach-dir) "/" a))

(require 'ob-shell)
(require 'org-docview)

(provide 'tz-org-init)
;;; tz-org-init.el ends here
