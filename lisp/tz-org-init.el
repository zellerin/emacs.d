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
  '(progn
     (setq org-confirm-babel-evaluate nil)
     (org-babel-lob-ingest "../emacs-setup.org")))

(eval-after-load "org-src"
  '(progn
     (push '("dot" . graphviz-dot) org-src-lang-modes)))

(setq org-babel-load-languages
      '((lisp . t)
	(dot . t)
	(emacs-lisp . t)
	(shell . t))
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
     (setq org-agenda-files '("~/org/")
	   org-agenda-todo-ignore-scheduled 'future
	   org-scheduled-past-days 1
	   org-agenda-custom-commands nil
	   org-agenda-prefix-format
	   ; show parents of todo list
	   '((agenda . " %i %-12:c%?-12t% s")
	     (timeline . "  % s")
	     (todo .
		   " %i %-12:c %b")
	     (tags .
		   " %i %-12:c %b")
	     (search . " %i %-12:c")))))

  (defun tz--capture-entry (letter name template &rest args)
    `(,letter ,name entry
 	      (file "weekly-review.org")
	      ,template
	      ,@args
	      :prepend t :clock-in t :clock-resume t :empty-lines 1))

(eval-after-load "org-capture"
  '(setq org-capture-templates
	 (list
	  ; todo
	  (tz--capture-entry "t" "TODO"		"* TODO %?\n%t%^{CATEGORY}p\n")
	  (tz--capture-entry "-" "Interruption"	"* %?\n%T\n")
	  (tz--capture-entry "j" "journal item" "* %? :journal:\n%t\n")
	  (tz--capture-entry "m" "Flag mail"	"* %:subject\n%a\n")
	  (tz--capture-entry "f" "Flag place"	"* %?\n%T\n%A\n")
	  (tz--capture-entry "r" "Remind person"
			     "* %:name\n%a\n")
	  (tz--capture-entry "w" "Remind web"
	   "* %(tz-capture-from-eww)%^g\n%T\n\n%(tz-eww-url)\n"))
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

(defun tz-capture-from-eww ()
  (save-excursion
    (set-buffer (get-buffer "*eww*"))
    (plist-get eww-data :title)))

(defun tz-eww-url ()
  (save-excursion
    (set-buffer (get-buffer "*eww*"))
    (concat "[[" (eww-current-url) "][link]] "
	    (thing-at-point 'sentence t))))

(eval-after-load "org-attach"
  '(setq org-attach-method 'mv))

(defun org-attached-tag (a)
  "Returns path to file in attach directory. To be used in a link abbreviation."
  (concat (org-attach-dir) "/" a))

(defcustom tz-dropbox-secret
  ""
  "Secret for org-mobile"
  :type 'string)

(defun tz-org-mobile-pre-pull ()
  (start-process "curl" "*MOBILE-TO-DROPBOX*"
		   "curl"
		   "-o" (concat org-mobile-directory "/mobileorg.org")
		   "https://content.dropboxapi.com/2/files/download"
		   "--header" (concat "Authorization: Bearer " tz-dropbox-secret)
		   "--header" "Dropbox-API-arg:  {\"path\": \"/Apps/MobileOrg/mobileorg.org\"}"))

(defun tz-push-to-dropbox (file)
  (interactive "f")
  (let ((url-request-data)
	(url-request-method "POST")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat  "Bearer " tz-dropbox-secret))
	   ("Dropbox-API-Arg" .
	    ,(concat "{\"path\": \"/Apps/MobileOrg/" file ".org\", \"mode\":\"overwrite\"}"))
	   ("Content-Type" . "application/octet-stream"))))
    (url-retrieve  "https://content.dropboxapi.com/2/files/upload"
		   (lambda (status)
		     (message "%s" status)))))

(defun tz-org-mobile-post-push ()
  (dolist (file '("agendas" "index" "weekly-review" "knowledgebase"))
    (tz-push-to-dropbox (concat org-mobile-directory "/" file ".org"))))

(add-hook 'org-mobile-post-push-hook 'tz-org-mobile-post-push)
(add-hook 'org-mobile-pre-pull-hook 'tz-org-mobile-pre-pull)

(require 'org-docview)

(provide 'tz-org-init)
;;; tz-org-init.el ends here
