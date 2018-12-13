(defun zap-in-kali ()
  "Run Zed Application Proxy in (possibly new) Kali buffer."
  (interactive)
  (kali)
  (comint-send-string nil "\nhostname localhost\nDISPLAY=:0 zaproxy &\n"))

(bind-key "<f12>Z" 'zap-in-kali)
