;;;###autoload
(defun gnus-only-emacs ()
  "Run new emacs with gnus only."
  (interactive)
  (start-process "*gnus*" "*emacs gnus*"
		 "emacs"
		 "--name" "GNUS"
		 "--eval" "(setq server-name \"gnus\")"
		 "--eval" "(add-hook 'emacs-startup-hook 'tz-gnus-init t)"))

;;;###autoload
(defun tz-gnus-init ()
  (interactive)
  (load-theme 'tz-gnus)
  (gnus)
  (gnus-demon-add-handler 'gnus-demon-scan-mail 30 10)
  (gnus-demon-add-rescan)
  (gnus-demon-init))
