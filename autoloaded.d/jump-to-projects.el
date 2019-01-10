;;;###autoload
(defun tz-org-jump-top-projects ()
  (interactive)
  (if (get-buffer "Projects list")
      (switch-to-buffer "Projects list")
    (find-file (org-link-expand-abbrev "org:/admin.org"))
    (outline-show-all)
    (switch-to-buffer (make-indirect-buffer (current-buffer) "Projects list" t))
    (goto-char (point-min))
    (search-forward "#+RESULTS:")
    (org-narrow-to-element)))

;;;###autoload
(bind-key (kbd "<f12> p") 'tz-org-jump-top-projects)
