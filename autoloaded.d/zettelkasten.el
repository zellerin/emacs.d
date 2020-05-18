;;;###autoload
(defun zettelkasten-card-name ()
  "Prepare name for zettelkasten card file based on name.

  Kill name so that it can be reused in the capture template.

  See [[id:424fc789-e55b-406d-93b6-83b8fc9c1f3e][refile to zettelkasten.]]"
  (let ((name  (read-string "Item: ")))
    (with-temp-buffer
      (insert name)
      (kill-region (point-min) (point-max)))
    (concat "~/org-roam/" name " "
	    (format-time-string "%Y-%m-%d")
	    ".org")))

;;;###autoload
(defun zettelkasten-insert-reference ()
  "Ask user for a refile target (that should include all zettelkasten
  methods) and insert to the current buffer link to it with description."
  (interactive)
  (let ((dest (org-refile-get-location))
	name id)
    (save-window-excursion
      (find-file (cadr dest))
      (goto-char (nth 3 dest))
      (setq id (org-id-get (point) t)
	    name (org-get-heading t t t t)))
    (org-insert-link nil (concat "id:" id) name)))

;;;###autoload
(defun zettelkasten-get-backlinks ()
  (interactive)
  (setq org-agenda-text-search-extra-files
	(directory-files "~/org-roam" t "\.org$"))
  (org-search-view nil (concat "id:" (org-id-get (point) t))))

;; Local Variables:
;; nameless-current-name: "zettelkasten"
;; End:
