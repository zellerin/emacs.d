;;; seems not to work
;;;###autoload
(defun tz-execute-startup-block ()
  (interactive)
  (let ((old-modified (buffer-modified-p)))
    (with-demoted-errors "Running startup block: %s"
      (org-babel-goto-named-src-block "startup")
      (org-babel-execute-src-block)
      (org-babel-remove-result))
    (set-buffer-modified-p old-modified)))

;;;###autoload
(with-eval-after-load "org"
  (add-hook 'tz-execute-startup-block 'org-mode-hook))
