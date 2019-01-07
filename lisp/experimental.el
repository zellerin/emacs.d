
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
		 :args ("/sbin/wpa_supplicant" "-i" "wlan" "-c" "/etc/wpa_supplicant/wpa_supplicant.conf")
		 :ready-message "CTRL-EVENT-CONNECTED"
		 :stop-signal 'sigkill
		 :on-output (lambda (&rest args)
			      (let ((output (plist-get args :output))
				 (service (plist-get args :service)))
				(when (s-matches? "CTRL-EVENT-DISCONNECTED" output)
				  (prodigy-set-status service 'interrupted)))))
	  (:name "dhclient"
		 :command "sudo"
		 :args ("dhclient" "-d" "-i" "wlan")
		 :ready-message "bound to "
		 :on-output (lambda (&rest args)
			      (let ((output (plist-get args :output))
				    (service (plist-get args :service)))
				(when (s-matches? "receive_packet failed on " output)
				  (prodigy-set-status service 'interrupted))))))))

(defun execsnoop ()
  (interactive)
  (switch-to-buffer
   (set-buffer (make-comint "execsnoop" "sudo" nil "/usr/share/bcc/tools/execsnoop"))))

(defun tcpconnect ()
  (interactive)
  (switch-to-buffer
   (set-buffer (make-comint "tcpconnect" "sudo" nil "/usr/share/bcc/tools/tcpconnect"))))

(defun systemd-journal ()
  (interactive)
  (switch-to-buffer (make-comint "journal" "journalctl" nil "-f")))

(with-eval-after-load 'org
  (setq org-link-abbrev-alist
	`(("attach" . org-attached-tag)))
  (logical-pathnames-org-insinuate)
  (add-hook 'org-mobile-post-push-hook 'tz-org-mobile-post-push)
  (add-hook 'org-mobile-pre-pull-hook 'tz-org-mobile-pre-pull))

(mapc 'load (directory-files "~/.emacs.d/lisp/experimental.d/" t "\\.el"))

(message "Time so far: %.1f secs" (float-time (time-subtract nil before-init-time)))

(let ((generated-autoload-file "~/.emacs.d/autoloaded.d/autoloads.el"))
  (when t
    (update-directory-autoloads
     "~/.emacs.d/autoloaded.d/"
     "~/.emacs.d/local/"))
  (message "Time so far: %.1f secs" (float-time (time-subtract nil before-init-time)))
  (load generated-autoload-file t))

(add-to-list 'load-path "~/.emacs.d/autoloaded.d/")

(provide 'experimental)
;;; experimental.el ends here
