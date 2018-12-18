(defun kali-rw ()
  "Run systemd container with Kali in a buffer."
  (interactive)
  (let ((default-directory "/sudo::/"))
    (switch-to-buffer
     (make-comint "kali" "systemd-nspawn"
		  nil  "--machine" "kali" "-D" "/opt/kali" "--bind-ro=/tmp/.X11-unix"))))

(defun kali-cmd (name &rest cmd)
  (let ((default-directory "/sudo::/"))
    (switch-to-buffer
     (apply 'make-comint name "systemd-nspawn"
	    nil "--machine" "kali-msf" "--read-only" "-D" "/opt/kali" "--bind-ro=/tmp/.X11-unix" "--bind=/tmp/:/root"
	    cmd))))

(defun kali-msf ()
  "Run systemd container with Metasploit (Kali) in a buffer."
  (interactive)
  (kali-cmd "kali-msf" "msfconsole" "-q"))

(defun kali-zap ()
  "Run systemd container with ZAP (Kali) in a buffer."
  (interactive)
  (kali-cmd "kali-zap" "zaproxy"))

(defun kali-sqlmap (&rest cmds)
  "Run systemd container with sqlmap (Kali) in a buffer."
  (apply 'kali-cmd "kali-sqlmap" "sqlmap" cmds))

(defun kali-sqlmap (&rest cmds)
  "Run systemd container with sqlmap (Kali) in a buffer."
  (apply 'kali-cmd "kali-sqlmap" "sqlmap" cmds))

(define-minor-mode nmap-mode "Mode for nmap"
  nil " nmap" nil
  (font-lock-add-keywords nil `(("[0-9]+/.* open .*" 0 '(:foreground "green"))
			("[0-9]+/.*closed.*" 0 '(:strike-through t))
			("Nmap scan report for .*" 0 '(bold t :overline t :height 150))
			("Nmap done.*" 0 '(:underline t :foreground "gray"))
			("^SF[-:].*" 0 '(:foreground "gray"))
			(,(regexp-opt '("Spawning container kali-msf on "
					"Press ^] three times within 1s to kill container."
					"Starting Nmap "
					"Container kali-msf exited successfully."))
			 0 '(:foreground "gray")))))

(defun kali-nmap (&rest cmds)
  "Run systemd container with sqlmap (Kali) in a buffer."
  (apply 'kali-cmd "kali-nmap" "nmap" cmds)
  (nmap-mode 1))
