(defun org-attach-dir-or-ask ()
  (interactive)
  "Simplify creating attach directories with nicer names."
  (or (org-attach-dir)
      (let ((new-name
	     (concat org-attach-directory "/"
		     (read-string "Attach dir name: " ))))
	(org-set-property "ATTACH_DIR" new-name)
	new-name)))
