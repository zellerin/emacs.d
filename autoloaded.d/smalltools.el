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

;;;###autoload
(defun systemd-journal ()
  (interactive)
  (switch-to-buffer (make-comint "journal" "journalctl" nil "-f")))
