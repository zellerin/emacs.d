
;;;###autoload
(defun nm-wifi ()
  "Provide interface for network manager"
  (interactive)
  (start-process "nmcli" "*nm*" "nmcli" "dev" "wifi")
  (switch-to-buffer "*nm*")
  (nm-mode)
  (goto-char 1))

(defun nm-wifi-connect (&optional name)
  (interactive
   (list
    (read-string "SSID: " (symbol-at-point))))
  (start-process "connect" "*wifi*" "nmcli" "dev" "wifi" "connect" name))

(define-derived-mode nm-mode fundamental-mode
  "Network Manager mode"
  (read-only-mode)
  (setq font-lock-defaults
	`((("WPA1" 0 'font-lock-warning-face)
	    "[0-9]+ Mbit/s" "WPA2" "802.1X"
	    ("^  \\(.*\\) \\(Infra\\)"
	     (1 '(face font-lock-variable-name-face))
	     (2 font-lock-keyword-face))
	    ("^\\*  \\(.*\\) \\(Infra\\)"
	     (1 '(face bold))
	     (2 font-lock-keyword-face))
	    ("\\<connected\\>" 0 '(face bold))
	    ("^NAME"
	     (0 'font-lock-keyword-face)
	     ("UUID\\|TYPE\\|DEVICE" nil nil (0 font-lock-keyword-face)))
	    (,(regexp-opt nm-connections-list 'words) .
	     (0 '(face bold connection t))
	     ))))
  (font-lock-mode))

(defcustom nm-connections-list
  '("empl" "eno1" "eno1emp")
  "List of nmcli connections to offer up/down"
  :group 'experimental
  :type '(repeat string))

(setq nm-mode-map
      '(keymap
	(?u . nm-connection-up)
	(?d . nm-connection-down)
	(?c . nm-connections)
	(?x . nm-devices)
	(?w . nm-wifi)
	(?? . describe-mode)))

(defun nm-connections ()
  "Provide interface for network manager"
  (interactive)
  (start-process "nmcli" "*nm*" "nmcli" "conn")
  (switch-to-buffer "*nm*")
  (nm-mode))

(defun nm-devices ()
  "Provide interface for network manager"
  (interactive)
  (start-process "nmcli" "*nm*" "nmcli" "dev")
  (switch-to-buffer "*nm*")
  (nm-mode)
  (goto-char 1))

(defun nm-connection-up (&optional conn)
  "Provide interface for network manager"
  (interactive)
  (setq conn
	(or conn (if (get-text-property (point) 'connection)
		     (word-at-point)
		   (read-string "Connection: "))))
  (start-process "nmcli" "*nm*" "nmcli" "conn" "up" conn)
  (switch-to-buffer "*nm*"))

(defun nm-connection-down (&optional conn)
  "Provide interface for network manager"
  (interactive)
  (setq conn
	(or conn (if (get-text-property (point) 'connection)
		     (word-at-point)
		   (read-string "Connection: "))))

  (start-process "nmcli" "*nm*" "nmcli" "conn" "down" conn)
  (switch-to-buffer "*nm*")
  (nm-mode))
