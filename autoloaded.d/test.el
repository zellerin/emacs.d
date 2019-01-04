;;;###autoload
(defun after-init-test ()
  (interactive)
  "Run tests of emacs configuration to make sure it at least starts up."
  (message "Startup time: %.1f secs"
	   (float-time (time-subtract after-init-time before-init-time)))
  (html-printscreen-buffer "init.html")

  (save-excursion
    (switch-to-buffer "*Warnings*")
    (html-printscreen-buffer "warnings.html")
    (kill-buffer))
  (save-excursion
    (org-agenda-list)
    (html-printscreen-buffer "agenda.html")
    (kill-buffer))
  (save-excursion
    (org-todo-list)
    (html-printscreen-buffer "todo.html")
    (kill-buffer))
  (message "Time so far: %.1f secs" (float-time (time-subtract nil before-init-time)))
  (save-excursion
    (switch-to-buffer "*Messages*")
    (html-printscreen-buffer "messages.html")
    (kill-buffer)))

(defvar tz-emacs-printscreen-dir
  (locate-user-emacs-file "tests/")
  "Directory for test case results")

(defun html-printscreen-buffer (target &optional dir)
  (interactive "G")
  (save-excursion
    (switch-to-buffer (htmlize-buffer))
    (write-file (concat (or dir tz-emacs-printscreen-dir) target))
    (kill-buffer)))
