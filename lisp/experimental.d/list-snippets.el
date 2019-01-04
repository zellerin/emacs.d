;;;###autoload
(defun tz-list-experimental-snippets ()
  (interactive)
  "List overview of experimental snippets to a buffer"
  (switch-to-buffer "*snippets*")
  (tabulated-list-mode)
  (setq tabulated-list-entries nil)
  (dolist (dir '(("~/.emacs.d/lisp/experimental.d/" . #("E" 0 1 (help-echo "Experimental")))
		 ("~/.emacs.d/autoloaded.d/" . #("A" 0 1 (help-echo "Autoloaded")))
		 ("~/.emacs.d/local" . #("L" 0 1 (help-echo "Local")))))
    (dolist (file (directory-files (car dir)
				   t "^[^.].*el$"))
      (unless (equal "autoloads" (file-name-base file))
	(let ((fn "") docstring (flags (cdr dir)))
	  (with-temp-buffer
	    (insert-file file)
	    (goto-char 1)
	    (when (re-search-forward "(\\(?:defun\\|define-[a-zA-Z-]*\\) \\([a-zA-Z-]+\\)" nil t)
	      (setq fn (match-string 1))
	      (when (re-search-forward "^ *\"\\([^\"\^J]+\\)" nil t)
		(setq docstring (match-string 1)))
	      (when (re-search-forward "bind-key" nil t)
		(setq flags (concat #("K" 0 1 (help-echo "Has key binding")) flags)))))
	  (push (list file
		      (vector (list (file-name-base file) 'file file 'action
				    (lambda (x) (find-file (get-text-property (point) 'file))))
			      (list fn 'face font-lock-variable-name-face)
			      flags
			      (list (or docstring "N/A") 'face
				    (if docstring font-lock-doc-face font-lock-keyword-face))))
		tabulated-list-entries)))))
  (setq tabulated-list-entries (nreverse tabulated-list-entries))
  (setq tabulated-list-format (vector
			       '("File" 20 nil)
			       '("Top function" 30 nil)
			       '("Flags" 4 nil)
			       '("Docstring" 50 nil)))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(bind-key "<f12>S" 'tz-list-experimental-snippets)
