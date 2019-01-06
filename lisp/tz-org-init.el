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
  (let ((url-request-data (save-excursion
			    (find-file-literally
			     (concat org-mobile-directory file))
			    (prog1 (buffer-string)
			      (kill-buffer))
			    ))
	(url-request-method "POST")
	(url-request-extra-headers
	 `(("Authorization" . ,(concat  "Bearer " tz-dropbox-secret))
	   ("Dropbox-API-Arg" .
	    ,(concat "{\"path\": \"/Apps/MobileOrg/" file "\", \"mode\":\"overwrite\"}"))
	   ("Content-Type" . "application/octet-stream"))))
     (url-retrieve  "https://content.dropboxapi.com/2/files/upload"
		   (lambda (status)
		     (message "%s" status)
		     (message "%s" (buffer-string))))))

(defun tz-org-mobile-post-push ()
  (dolist (file '("agendas" "index" "weekly-review" "knowledgebase"))
    (tz-push-to-dropbox (concat file ".org")))
  (tz-push-to-dropbox "checksums.dat"))

(add-hook 'org-mobile-post-push-hook 'tz-org-mobile-post-push)
(add-hook 'org-mobile-pre-pull-hook 'tz-org-mobile-pre-pull)

(require 'org-docview)

(provide 'tz-org-init)
;;; tz-org-init.el ends here
