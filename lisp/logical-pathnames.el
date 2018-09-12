;;; logical-pathnames.el --- Shorten long pathnames  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Tomas Zellerin

;; Author: Tomas Zellerin <tzellerin@gmail.com>
;; Version: 0.9
;; Keywords: files
;; Package-Requires: ((emacs "24.4"))
;; Url: https://github.com/zellerin/emacs-logical-pathnames

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

;; Logical pathnames are a concept from Common Lisp.  The primary
;; purpose is to allow portable specification of paths that can be
;; used across systems.  This package is (very loosely) based on it.

;; While some of fixes it provides in CL are not relevant now
;; (versioning filesystems, case conversion) or on Emacs (different
;; path separators), files are still on different location on
;; different systems, in particular on Linux and Windows.

;; Also, some Windows paths (especially on shared disks) are so long
;; that you do not really want to see them.

;; This package allows to use short names that expand to prefixes on
;; some places, namely:
;; - org links

;; and lets the system replace the long names with short ones on other places, namely:
;; - dired buffers
;; - stored org links

;; So some of the use cases when you might want to use this package are:
;; - You want to be able to say "projects:client.org" in org file and let the projects directory be on different place on different system you use,
;; - You want to be able to say "shared:file.txt" and do not bother will full path to shared disk (SMB share? Tramp link? whatever)
;; You can also move the defined directories around and update the path just once for the mapping.

;; Configuration: customize `logical-pathnames-names'.

;; Setup:
;;   Call `logical-pathnames-org-insinuate' to set up org abbreviations and make  `org-store-link' use shortened paths.
;;   Add `logical-pathnames-update-buffer' to `dired-after-readin-hook' to see shortened paths in dired module

;;; Code:

(defcustom logical-pathnames-names
  '(("org" . "~/org")
    ("emacs" . "~/.emacs"))
  "List of mapping from short name to a path.
It is currently used for org mode abbreviations, and for nice view of files in the dashboard."
  :type '(alist :key-type string :value-type directory))

;;;###autoload
(defun logical-pathnames-update-buffer ()
  "Change all references in a buffer to short name.
Can be used in `dired-after-readin-hook'"
  (interactive)
  (save-excursion
    (let ((inhibit-read-only t))
      (dolist (ldisk logical-pathnames-names)
	(goto-char 1)
	(while (search-forward (cdr ldisk) nil t)
	  (replace-match (concat (car ldisk) ":")))))))

(defun logical-pathnames-update-link (&rest pars)
  "Make last link from org mode links logical.

Suitable for hook on `org-store-link'; for this reason, it
formally accepts PARS (but does not use them)."
  (when (car org-stored-links)
    (let* ((link (caar org-stored-links))
	   (res link))
      (dolist (ldisk logical-pathnames-names)
	;; c:/ should be kept
	(unless (cl-find ?\: (substring (cdr ldisk) 2))
	  (setf ldisk (cons (car ldisk) (concat "file:" (cdr ldisk)))))
	(when (or (string-prefix-p (cdr ldisk) link))
	  (setq link (concat (car ldisk) ":" (substring link (length (cdr ldisk)))))))
      (setf (car org-stored-links) (cons link (cdar org-stored-links))))))

;;;###autoload
(defun logical-pathnames-org-insinuate ()
  "Make org links aware of logical pathnames."
  (advice-add 'org-store-link :after #'logical-pathnames-update-link)
  (dolist (ln logical-pathnames-names)
    (push (cons (car ln) (org-link-expand-abbrev (cdr ln)))
	  org-link-abbrev-alist)))

(provide 'logical-pathnames)
;;; logical-pathnames.el ends here
