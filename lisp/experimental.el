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

(provide 'experimental)
;;; experimental.el ends here
