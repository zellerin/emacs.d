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
