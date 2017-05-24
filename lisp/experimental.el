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
(use-package "slime"
  :config (setq slime-contribs '(slime-fancy)
		slime-net-coding-system (quote utf-8-unix))
  :defer t)

(defvar tz-logical-names
  '(("docs" . "c:/Users/tzellerin/documents/")
    ("conf" . "c:/Users/tzellerin/configs/")))

(setq org-link-abbrev-alist
      `(,@tz-logical-names))

(defun tz-pathnames-logical ()
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (dolist (ldisk tz-logical-names)
	(goto-char 1)
	(while (search-forward (cdr ldisk) nil t)
	  (replace-match (concat (car ldisk) ":")))))))

(defun tz--insert-file-top (file n)
  "Insert N lines starting with second one (first may be a
modeline) to current buffer."
  (insert
   (or
    (save-current-buffer
      (when (file-readable-p file)
	(find-file file)
	(goto-char (point-min))
	(forward-line 1)
	(buffer-substring (point)
			  (progn (setq n (forward-line n))
				 (point)))))
    (format "Create ~s" file)))
  n)

(defun dashboard-insert-tips (n)
  (tz--insert-file-top "~/.emacs.d/tips.org"
		       (tz--insert-file-top "~/tips.org" n)))

(use-package "dashboard"
  :config
  (add-hook 'dashboard-mode-hook 'tz-pathnames-logical)

  (add-to-list 'dashboard-item-generators  '(tips . dashboard-insert-tips))
  (add-to-list 'dashboard-items '(tips) t)
  (dashboard-setup-startup-hook))

(setq recentf-exclude '("emacs.d/elpa/" "/emacs/[0-9.]*/lisp/"))

(setq projectile-switch-project-action #'projectile-dired)

(provide 'experimental)
;;; experimental.el ends here
