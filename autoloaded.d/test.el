;;;###autoload
(defun after-init-test ()
  (interactive)
  "Run tests of emacs configuration to make sure it at least starts up."
  (message "Startup time: %.1f secs"
	   (float-time (time-subtract after-init-time before-init-time)))
  (html-save-buffer)

  (save-excursion
    (switch-to-buffer "*Warnings*")
    (html-save-buffer)
    (kill-buffer))
  (save-excursion
    (org-agenda-list)
    (html-save-buffer)
    (kill-buffer))
  (save-excursion
    (org-todo-list)
    (html-save-buffer)
    (kill-buffer))
  (message "Time so far: %.1f secs" (float-time (time-subtract nil before-init-time)))
  (save-current-buffer
    (switch-to-buffer "*Messages*")
    (html-save-buffer)
    (kill-buffer))
  (html-save-result "~/.emacs.d/tests/testlog.html"))

(defun html-save-buffer ()
  (interactive)
  (let ((content (buffer-string))
	(name (buffer-name)))
    (save-current-buffer
      (switch-to-buffer "*result log*")
      (insert "\n---" name "---\n" content))
    (kill-buffer)))

(defun html-save-result (target)
  (switch-to-buffer "*result log*")
  (switch-to-buffer (htmlize-buffer))
  (write-file target)
  (kill-buffer))
