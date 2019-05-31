;;;###autoload
(defun lisp-run-tests ()
  (interactive )
  (sly-eval-async
      '(cl:with-output-to-string
	(cl:*standard-output*)
	(rt:do-tests))
    (lambda (res)
      (small-tools-command-buffer "RT results"
				  '(("No tests failed." (0 '(:foreground "green")))
				    ("Doing \\([0-9]*\\) pending tests? of \\([0-9]*\\) tests total."
				     (0 'font-lock-keyword-face)
				     (1 '(font-lock-keyword-face bold t) t)
				     (2 '(font-lock-keyword-face bold t) t))
				    )
				  (lambda ()
				    (insert res)
				    (setq small-tools-buffer-revert-commands 'lisp-run-tests))))))
