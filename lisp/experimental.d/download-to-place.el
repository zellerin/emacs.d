;;; download-to-place.el --- Download in org mode    -*- lexical-binding: t; -*-

(defun tz-download-at-point (&optional url)
  (interactive)
  (unless url
    (setq url (read-string "Url: " (thing-at-point 'url))))
  (url-retrieve
   (org-link-expand-abbrev url)
   'tz-download-callback
   (list (concat "~/Downloads/" (file-name-nondirectory url)))))

(defun tz-download-callback (status file)
  (unless (plist-get status :error)
    (goto-char (point-min))
    (re-search-forward "\r?\n\r?\n")
    (let ((coding-system-for-write 'no-conversion))
      (write-region (point) (point-max) file))
    (message "Saved %s" file)
    (find-file-other-frame file)))

(defun org-attach-downloaded-link ()
  "Download linked element to the attachments."
  (interactive)
  (let ((context
	 (org-element-lineage (org-element-context) '(link) t)))
    (when context
      (url-retrieve (cl-getf (cadr context) :raw-link)
		    'tz-download-callback
		    (list (concat
			   (org-attach-dir t) "/"
			   (file-name-nondirectory
			    (cl-getf (cadr context) :raw-link))))))))

(eval-after-load 'org
  '(bind-key "<f12>D" 'org-attach-downloaded-link org-mode-map))
(bind-key "<f12>D" 'tz-download-at-point)
