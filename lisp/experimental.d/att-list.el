(defun tz-insert-list-of-attachments ()
  (interactive)
  (mapcar (lambda (a)
	    (insert (format "- [[%s][%s]]\n" a (file-name-nondirectory a))))
	  (directory-files (org-attach-dir) t "^[^.]")))

(eval-after-load 'org
  '(bind-key "<f12>A" 'tz-insert-list-of-attachments org-mode-map))
