;;; BPF scripts (Linux only)

;;;###autoload
(defun execsnoop ()
  (interactive)
  (switch-to-buffer
   (set-buffer (make-comint "execsnoop" "sudo" nil "/usr/share/bcc/tools/execsnoop"))))

;;;###autoload
(defun tcpconnect ()
  (interactive)
  (switch-to-buffer
   (set-buffer (make-comint "tcpconnect" "sudo" nil "/usr/share/bcc/tools/tcpconnect"))))

;;; Systemd journal
;;;###autoload
(defun systemd-journal (arg)
  (interactive "P")
  (message "%s" arg)
  (apply 'start-process "journal" "*journal*"  "journalctl"
	 "-q"
	 (if arg `("-p" ,(number-to-string (if (consp arg) (car arg) arg)))
	   '("-f")))
  (switch-to-buffer "*journal*")
  (read-only-mode))
