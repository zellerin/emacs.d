;;;###autoload
(defun run-with-sane-print-convention (fn)
  "Ensure ' and \\n are used when function prints,  not quote and hard newline."
  (let ((print-quoted t)
	(print-escape-newlines t))
    (funcall fn)))

;;;###autoload
(advice-add 'custom-save-all :around 'run-with-sane-print-convention)
