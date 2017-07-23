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
(defcustom experimental-logical-names
  '(("org" . "~/org")
    ("conf" . "c:/Users/tzellerin/configs/"))
  "List of mapping from short name to a path. It is currently
  used for org mode abbreviations, and for nice view of files in the dashboard."
  :type '(alist :key-type string :value-type directory)
  :group 'tze)

(use-package "org"
  :config (setq org-link-abbrev-alist
	 `(,@experimental-logical-names)))

(defun experimental-pathnames-logical ()
  "Change all references to a pathnames mentioned in experimental-logical-names to the short name."
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (dolist (ldisk experimental-logical-names)
	(goto-char 1)
	(while (search-forward (cdr ldisk) nil t)
	  (replace-match (concat (car ldisk) ":")))))))

(use-package "dashboard"
  :bind ("<f5>" . dashboard-refresh-buffer)
  :config
  (add-hook 'dashboard-mode-hook 'experimental-pathnames-logical)

  (add-to-list 'dashboard-item-generators  '(tips . experimental-insert-tips))
  (add-to-list 'dashboard-items '(tips) t)
  (dashboard-setup-startup-hook)

  (defun experimental--insert-files-top (files n)
  "Insert N lines starting with second one (first may be a modeline) to current buffer."
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

  :demand t)

(use-package "prodigy"
  :defer t
  :config

  (prodigy-define-status :id 'interrupted :face 'prodigy-yellow-face)

  (setq prodigy-services
	'((:name "wpa-supplicant"
		 :command "sudo"
		 :args ("/sbin/wpa_supplicant" "-i" "wlo1" "-c" "/etc/wpa_supplicant/wpa_supplicant.conf")
		 :ready-message "CTRL-EVENT-CONNECTED"
		 :stop-signal 'sigkill
		 :on-output (lambda (&rest args)
			      (let ((output (plist-get args :output))
				 (service (plist-get args :service)))
				(when (s-matches? "CTRL-EVENT-DISCONNECTED" output)
				  (prodigy-set-status service 'interrupted)))))
	  (:name "dhclient"
		 :command "sudo"
		 :args ("dhclient" "-d" "-i" "wlo1")
		 :ready-message "bound to "
		 :on-output (lambda (&rest args)
			      (let ((output (plist-get args :output))
				    (service (plist-get args :service)))
				(when (s-matches? "receive_packet failed on " output)
				  (prodigy-set-status service 'interrupted))))))))

(provide 'experimental)
;;; experimental.el ends here
