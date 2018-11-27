(defun tz-list-experimental-snippets ()
  (interactive)
  "List overview of experimental snippets to a buffer"
  (switch-to-buffer "*snippets*")
  (tabulated-list-mode)
  (setq tabulated-list-entries nil)
  (dolist (file (directory-files "~/.emacs.d/lisp/experimental.d/"
				 nil ".el$"))
    (let (fn docstring (flags ""))
      (with-temp-buffer
	(insert-file file)
	(goto-char 1)
        (re-search-forward "(defun \\([a-zA-Z-]+\\)" nil t)
	(setq fn (match-string 1))
	(when (re-search-forward "^ *\"\\([^\"\^J]+\\)" nil t)
	  (setq docstring (match-string 1)))
	(when (re-search-forward "bind-key" nil t)
	  (setq flags "K")))
      (when fn
	(push (list file (vector (list file 'action
				      (lambda (x) (find-file (ffap-file-at-point))))
				 (list fn 'face font-lock-variable-name-face)
				 flags
				 (list (or docstring "N/A") 'face
				       (if docstring font-lock-doc-face font-lock-keyword-face))))
	      tabulated-list-entries))))
  (setq tabulated-list-entries (nreverse tabulated-list-entries))
  (setq tabulated-list-format (vector
			       '("File" 20 nil)
			       '("Top function" 30 nil)
			       '("Flags" 4 nil)
			       '("Docstring" 50 nil)))
  (tabulated-list-init-header)
  (tabulated-list-print))

(bind-key "<f12>S" 'tz-list-experimental-snippets)
