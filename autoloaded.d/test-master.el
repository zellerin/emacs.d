;;;###autoload
(defun tz-start-emacs-tests (keep-alive)
  "Run new emacs and start `after-init-tests' there.

Kills new emacs unless KEEP-ALIVE is set (or called with C-u interactively)"
  (interactive "P")
  (apply 'start-process "*test*" "*emacs test*"
	 "emacs"
	 "--eval" "(setq server-name \"foo\")"
	 "--eval" "(add-hook 'emacs-startup-hook 'after-init-test t)"
	 (unless keep-alive
	   '("--eval" "(add-hook 'emacs-startup-hook 'kill-emacs t)"))))
