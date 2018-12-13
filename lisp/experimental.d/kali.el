(defun kali ()
  "Run systemd container with Kali in a buffer."
  (interactive)
  (let ((default-directory "/sudo::/"))
    (switch-to-buffer
     (make-comint "kali" "systemd-nspawn"
		  nil  "--machine" "kali" "-D" "/opt/kali" "--bind-ro=/tmp/.X11-unix"))))
