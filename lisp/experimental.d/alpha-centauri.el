(defun alpha-centauri ()
  (interactive)
  (with-temp-buffer
    (let ((default-directory "/opt/compressed/drive_c/Program Files/Sid Meier's Alpha Centauri"))
      (start-process-shell-command "ALPHA" "ALPHA"
				      "/usr/bin/wine32 ./terran.exe"))))
