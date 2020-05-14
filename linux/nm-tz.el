;;;###autoload  -*- lexical-binding: t; -*-
(defun small-tools-nm-list-devices ()
  "Provide interface for network manager."
  (interactive)
  (small-tools-command "nm-devices" '("nmcli" "dev"))
  (nmcli-mode))

;;;###autoload
(defun small-tools-nm-list-connections ()
  "Provide interface for network manager."
  (interactive)
  (small-tools-command "nm-connections" '("nmcli" "conn"))
  (nmcli-mode))

(defun small-tools-line-connection-name ()
  (and (equal mode-name "nmcli")
       (save-excursion
	 (move-beginning-of-line nil)
	 (re-search-forward "[-a-f0-9]\\{36\\}")
	 (buffer-substring-no-properties (match-beginning 0) (match-end 0)))))

(define-derived-mode nmcli-mode special-mode "nmcli"
  "Mode for nmclient lists."
  (setq font-lock-defaults (list
		    '(("^NAME.*" (0 'font-lock-keyword-face))
		      (".*[^-\s ]\s *$" 0
		       `(face bold))
		      (".*--" 0
		       `(face default))))))

(bind-key "RET" 'nmcli-describe-connection nmcli-mode-map)
(bind-key "c" 'small-tools-nm-list-connections nmcli-mode-map)
(bind-key "d" 'small-tools-nm-list-devices nmcli-mode-map)
(bind-key "e" 'nmcli-edit-connection nmcli-mode-map)
(bind-key "k" 'small-tools-nm-connection-kill-maybe nmcli-mode-map)
(bind-key "g" 'small-tools-revert-buffer nmcli-mode-map)
(bind-key "+" 'small-tools-nm-connection-up nmcli-mode-map)
(bind-key "-" 'small-tools-nm-connection-down nmcli-mode-map)
(bind-key "w" 'nm-wifi nmcli-mode-map)

(defun nmcli-describe-connection (&optional connection)
  "Describe connect at point to a separate buffer."
  (interactive)
  (setq connection
	(or connection (small-tools-line-connection-name)))
  (small-tools-command-buffer connection nil `("nmcli" "conn" "show" ,connection))
  (conf-colon-mode))

(defun nmcli-edit-connection (&optional connection)
  (interactive)
  (setq connection
	(or connection (small-tools-line-connection-name)))
  (switch-to-buffer
   (make-comint connection "nmcli" nil "conn" "edit" connection)))

(defun small-tools-run-and-refresh (&rest commands)
  (make-process
   :buffer "debug"
   :name (car commands)
   :command commands
   :sentinel (lambda (_ state)
	       (message "%S: %s" commands (string-trim-right state))
	       (small-tools-revert-buffer))))

(cl-macrolet ((@ (name args string &rest commands)
		 `(defun ,name ,args ,string
			 (interactive)
			 (small-tools-run-and-refresh ,@ commands))))
  (@ small-tools-nm-connection-up (&optional conn)
		       "Provide interface for network manager"
		     "nmcli" "conn" "up" (or conn (small-tools-line-connection-name)))
  (@ small-tools-nm-connection-down (&optional conn)
			 "Provide interface for network manager"
			 "nmcli" "conn" "down" (or conn (small-tools-line-connection-name)))
  (@ small-tools-nm-connection-kill (&optional conn)
			 "Provide interface for network manager"
			 `("sudo" "nmcli" "conn" "delete" ,(or conn (small-tools-line-connection-name)))))

(define-derived-mode nm-wifi-mode nmcli-mode
  "nmcli wifi list"
  "Mode for wifi connection list."
  (setq font-lock-defaults
	`(((,(regexp-opt
	      '("IN-USE" "SSID" "MODE" "CHAN" "RATE" "SIGNAL" "BARS" "SECURITY"))
	    . font-lock-comment-face)
	   ("WPA1" 0 'font-lock-warning-face)
	     "[0-9]+ Mbit/s" "WPA2" "802.1X"
	     ("^  \\(.*\\) \\(Infra\\)"
	      (1 '(face font-lock-variable-name-face))
	      (2 font-lock-keyword-face))
	     ("^\\*  \\(.*\\) \\(Infra\\)"
	      (1 '(face bold))
	      (2 font-lock-keyword-face))))))

;;;###autoload
(defun nm-wifi ()
  "Provide interface for network manager"
  (interactive)
  (small-tools-command "wifi" '("nmcli" "dev" "wifi"))
  (nm-wifi-mode))

(defun nm-wifi-connect (&optional name password)
  (interactive
   (list
    (read-string "SSID: " (symbol-at-point))
    (read-string "password: " )))
  (small-tools-run-and-refresh "nmcli" "dev" "wifi" "connect"
			       name "password" "password"))

(define-derived-mode nm-mode fundamental-mode
  "Network Manager mode"
  (read-only-mode)

  (font-lock-mode))

(bind-key "+" 'nm-wifi-connect nm-wifi-mode-map)

(defun nm-connection-up (&optional conn)
  "Provide interface for network manager"
  (interactive)
  (setq conn
	(or conn (if (get-text-property (point) 'connection)
		     (word-at-point)
		   (read-string "Connection: "))))
  (start-process "nmcli" "*nm*" "nmcli" "conn" "up" conn)
  (switch-to-buffer "*nm*"))
