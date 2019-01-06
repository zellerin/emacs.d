;;;###autoload
(defun org-attach-dir-or-ask ()
  (interactive)
  "Simplify creating attach directories with nicer names."
  (or (org-attach-dir)
      (let ((new-name
	     (concat org-attach-directory "/"
		     (read-string "Attach dir name: " ))))
	(org-set-property "ATTACH_DIR" new-name)
	new-name)))

;;;###autoload
(defun tz-get-org-title ()
  "Get TITLE of org file."
  (save-excursion
    (goto-char 1)
    (if (re-search-forward "#\\+TITLE:\\s *\\(.*\\)" nil t)
      (match-string 1)
      (buffer-name))))

; also possible
; (defun get-keyword (key)
;   (org-element-map (org-element-parse-buffer) 'keyword
;     (lambda (k)
;       (when (string= key (org-element-property :key k))
; (org-element-property :value k)))
;     nil t))

;;;###autoload
(defun org-nice-bullets ()
  (interactive)
  (font-lock-add-keywords nil
		  '(("^ *\\([-]\\) "
		     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

;;;###autoload
(defun tz-export-subtree ()
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (let ((file (org-id-get-create))
	  (has-name (org-entry-get (point) "EXPORT_FILE_NAME")))
      (unless has-name
	(org-entry-put (point) "EXPORT_FILE_NAME" file))
      (save-excursion
	(org-export-as-html 4 nil nil nil nil "/tmp/"))
      (unless has-name (undo-only)))))

;;;###autoload
(defun org-attached-tag (a)
  "Returns path to file in attach directory. To be used in a link abbreviation."
  (concat (org-attach-dir) "/" a))
