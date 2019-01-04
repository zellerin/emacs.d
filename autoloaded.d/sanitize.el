;;;###autoload
(defun tz-sanitize-for-kb ()
  (interactive)
  (org-deadline '(4)) ; remove deadline and schedule
  (org-schedule '(4))
  (org-todo 'none) ; remove todo state
  (save-restriction
    (org-narrow-to-subtree)
    ;; remove babel code outputs
    (org-babel-remove-result-one-or-many t))
  (let ((org-refile-targets '(("knowledgebase.org" :maxlevel . 10)))
	(org-refile-use-outline-path 'file)
	(org-log-refile 'time)
	(org-refile-allow-creating-parent-node t))
    (org-refile nil nil nil "Move to KB: "))
  (org-refile '(16))
  (org-set-tags-command))

;;;###autoload
(eval-after-load 'org
  '(bind-key "<f12> R" 'tz-sanitize-for-kb org-mode-map))
