;;; experimental.el --- Experimental startup code    -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Tomas Zellerin

;; Author: Tomas Zellerin <zellerin@pluto>
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

;;;; Slime setup
(use-package "slime"
  :config (setq slime-contribs '(slime-fancy)
		slime-net-coding-system (quote utf-8-unix))
  :defer t)

(defcustom experimental-logical-names
  '(("org" . "~/org")
    ("conf" . "c:/Users/tzellerin/configs/"))
  "List of mapping from short name to a path. It is currently
  used for org mode abbreviations, and for nice view of files in the dashboard."
  :type '(alist :key-type string :value-type directory)
  :group 'tze)

(eval-after-load "org"
  '(setq org-link-abbrev-alist
	 `(,@experimental-logical-names)))

(defun experimental-pathnames-logical ()
  "Change all references to a pathnames mentioned in
experimental-logical-names to the short name."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (dolist (ldisk experimental-logical-names)
	(goto-char 1)
	(while (search-forward (cdr ldisk) nil t)
	  (replace-match (concat (car ldisk) ":")))))))

(defun experimental--insert-files-top (files n)
  "Insert N lines starting with second one (first may be a
modeline) to current buffer."
  (while (and files (> n 1))
    (let ((file (pop files)))
      (insert
       (or
	(save-current-buffer
	  (when (file-readable-p file)
	    (find-file file)
	    (goto-char (point-min))
	    (forward-line 1)
	    (prog1
		(buffer-substring (point)
				  (progn (setq n (forward-line n))
					 (point)))
	      (bury-buffer))))
	(format "Create %s" file))))))

(defun experimental-insert-tips (n)
  "Insert given number of tips from tip files to the buffer."
  (experimental--insert-files-top '("~/tips.org" "~/.emacs.d/tips.org") n))

(use-package "dashboard"
  :bind ("<f5>" . dashboard-refresh-buffer)
  :config
  (add-hook 'dashboard-mode-hook 'experimental-pathnames-logical)

  (add-to-list 'dashboard-item-generators  '(tips . experimental-insert-tips))
  (add-to-list 'dashboard-items '(tips) t)
  (dashboard-setup-startup-hook)
  :demand t)

(setq recentf-exclude '("emacs.d/elpa/" "/emacs/[0-9.]*/lisp/"))

(provide 'experimental)
;;; experimental.el ends here
