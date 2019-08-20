;;; Environment setup:
;;;

(defcustom kali-system-directory "/var/lib/machines/kali-work/"
  "Directory where Kali machine is stored"
  :type 'directory
  :group 'kali)

;;;###autoload
(defun kali-rw ()
  "Run systemd container with Kali in a buffer."
  (interactive)
  (let ((default-directory "/sudo::/"))
    (switch-to-buffer
     (make-comint "kali" "systemd-nspawn"
		  nil  "--machine" "kali" "-D"
		  "/var/lib/machines/kali-work" "--bind-ro=/tmp/.X11-unix"
		  "--bind" "/home/zellerin/Downloads:/root/Downloads")
     ;(setq comint-file-name-prefix "/sudo::/var/lib/machines/kali-work")
     )))

;;;###autoload
(defun kali-cmd (name &rest cmd)
  (make-directory "~/current/root/" t)
  (let ((default-directory "/sudo::/"))
    (switch-to-buffer
     (apply 'make-comint name "systemd-nspawn"
	    nil "--machine" name
	    "--read-only" "-D" (concat "/var/lib/machines/" name)
	    "--bind-ro=/tmp/.X11-unix"
	    "--bind=/home/zellerin/current/root:/root"
	    "--bind=/home/zellerin/current/:/root/current"
	    "--setenv" "DISPLAY=:0"
	    cmd))))

;;;###autoload
(defun kali-msf ()
  "Run systemd container with Metasploit (Kali) in a buffer."
  (interactive)
  (kali-cmd "kali-msf" "msfconsole" "-q"))

(defun kali-msf-cmd ()
  "Run systemd container with Metasploit (Kali) in a buffer."
  (interactive)
  (kali-cmd "kali-msf"))

;;;###autoload
(defun kali-zap ()
  "Run systemd container with ZAP (Kali) in a buffer."
  (interactive)
  (make-directory "~/current/zap" t)
  (kali-cmd "kali-zaproxy"
	    "--bind=/home/zellerin/current/zap:/root/.ZAP/"
	    "/usr/bin/zaproxy"))

(defun kali-sqlmap (&rest cmds)
  (interactive
   (list "-u"
	 (read-string "Url: ")))
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

;;;###autoload
(defun kali-nmap (&rest cmds)
  "Run systemd container with sqlmap (Kali) in a buffer."
  (apply 'kali-cmd "kali-nmap" "nmap" cmds)
  (nmap-mode 1))


(defun systemd-run-work ()
  "Run systemd container with Kali in a buffer."
  (interactive)
  (let ((default-directory "/sudo::/"))
    (switch-to-buffer
     (make-comint "kali" "systemd-nspawn"
		  nil  "--machine" "kali" "-D"
		  "/var/lib/machines/root-copy" "--bind-ro=/tmp/.X11-unix"
		  "--bind" "/home/zellerin/iltm-files:/root/iltm-files")
     ;(setq comint-file-name-prefix "/sudo::/var/lib/machines/kali-work")
     )))
