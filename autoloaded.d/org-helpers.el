;;;###autoload
(defun org-attach-dir-or-ask ()
  (interactive)
  "Simplify creating attach directories with nicer names."
;; Maybe it is easier to change the attach directory afterwards, now
;; that it copies the files? (org-attach-set-directory)
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
(defun tz-export-subtree ()
  (interactive)
  (save-excursion
    (org-mark-subtree)
    (let ((file (org-id-get-create))
	  (has-name (org-entry-get (point) "EXPORT_FILE_NAME")))
      (unless has-name
	(org-entry-put (point) "EXPORT_FILE_NAME" file))
      (save-excursion
	(org-open-file (org-html-export-to-html nil t)))
      (unless has-name (undo-only)))))

;;;###autoload
(defun org-attached-tag (a)
  "Returns path to file in attach directory. To be used in a link abbreviation."
  (concat (org-attach-dir) "/" a))

;;;###autoload
(with-eval-after-load "org"
  (font-lock-add-keywords 'org-mode '(("[-+] \\[ ] " (0 '(face fixed-pitch display ("☐ "))))
			      ("[-+] \\[X] " (0 '(face fixed-pitch display ("☑ "))))
			      ("^ *\\([-]\\) "
			       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))
			      ("^ *\\([+]\\) "
			       (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "▸")))))))
