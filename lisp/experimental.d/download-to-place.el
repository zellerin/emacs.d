;;; experimental.el --- Experimental startup code    -*- lexical-binding: t; -*-

(defun tz-download-callback (file-abbrev)
  (let ((file (org-link-expand-abbrev file-abbrev)))
    (lambda  (status)
      (unless (plist-get status :error)
	(goto-char (point-min))
	(re-search-forward "\r?\n\r?\n")
	(write-region (point) (point-max) file)
	(message "Saved %s" file)))))

(defun tz-download-at-point (&optional url)
  (interactive)
  (unless url
    (setq url (read-string "Url: " (thing-at-point 'url))))
  (url-retrieve
   url
   (tz-download-callback (concat "~/Downloads/" (file-name-nondirectory url)))))
