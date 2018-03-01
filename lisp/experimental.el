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

(defun execsnoop ()
  (interactive)
  (set-buffer (make-term "execsnoop" "sudo" nil "/usr/share/bcc/tools/execsnoop"))
  (term-mode)
  (switch-to-buffer "*execsnoop*"))

(use-package "org"
  :defer t
  :config
  (setq org-link-abbrev-alist
		`(("attach" . org-attached-tag)))
  (logical-pathnames-org-insinuate)
  (setq org-directory (org-link-expand-abbrev "org:"))
  (setq org-agenda-files
	(mapcar #'org-link-expand-abbrev
			 '("org:" "project:"))))

(provide 'experimental)
;;; experimental.el ends here
