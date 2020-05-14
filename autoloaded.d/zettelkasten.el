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
