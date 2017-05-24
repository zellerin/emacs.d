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
(add-hook 'org-mode-hook
	  (lambda ()
	    (push '(":INBOX:" . ?📥) prettify-symbols-alist)
	    (push '(":ATTACH:" . ?📎) prettify-symbols-alist)
	    (push '(":published:" . ?📢) prettify-symbols-alist)
	    (prettify-symbols-mode)))

(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (push '(":INBOX:" . ?📥) prettify-symbols-alist)
	    (push '(":ATTACH:" . ?📎) prettify-symbols-alist)
	    (push '(":published:" . ?📢) prettify-symbols-alist)
	    (prettify-symbols-mode)))

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

(add-hook 'dashboard-mode-hook 'tz-pathnames-logical)

(defun dashboard-insert-quicks (n)
  (insert "Quick tasks:\n")
  (insert
   (save-current-buffer
     (find-file "c:/Users/tzellerin/Documents/test.org")
        (goto-char 1)
        (buffer-substring (- (search-forward "\n- ") 2)
                          (- (search-forward "\n* ") 2)))))
(add-to-list 'dashboard-item-generators  '(quicks . dashboard-insert-quicks))
(add-to-list 'dashboard-items '(quicks) t)

(provide 'experimental)
;;; experimental.el ends here
