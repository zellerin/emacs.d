;;;###autoload
(defun tz-insert-list-of-attachments ()
  "Insert list of attachments of an org node"
  (interactive)
  (mapcar (lambda (a)
	    (insert (format "- [[%s][%s]]\n" a (file-name-nondirectory a))))
	  (directory-files (org-attach-dir) t "^[^.]")))

;;;###autoload
(eval-after-load 'org
  '(bind-key "<f12>A" 'tz-insert-list-of-attachments org-mode-map))
