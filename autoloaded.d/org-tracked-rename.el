;;;###autoload
(defun org-tracked-rename ()
  "Rename a header and make a note about it."
  (interactive)
  (org-kill-line)
  (save-excursion
    (goto-char (org-log-beginning t))
    (insert "   - Note taked on ")
    (org-time-stamp '(16) t)
    (insert " \\\\\n     Renamed from ")
    (yank)
    (insert "\n")))

;;;###autoload
(eval-after-load 'org
  '(bind-key "<f12> /" 'org-tracked-rename org-mode-map))
