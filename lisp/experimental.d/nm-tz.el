(define-derived-mode nm-mode fundamental-mode
  "Network Manager mode"
  "Provide interface for network manager"
  (font-lock-mode)
  (read-only-mode)
  (font-lock-add-keywords nil `(("WPA1" 0 'font-lock-warning-face)
			"[0-9]+ Mbit/s" "WPA2" "802.1X"
			("^  \\(.*\\) \\(Infra\\)"
			 (1 '(face font-lock-variable-name-face))
			 (2 font-lock-keyword-face))
			("^\\*  \\(.*\\) \\(Infra\\)"
			 (1 '(face bold))
			 (2 font-lock-keyword-face)))))

(defun nm-wifi ()
  (interactive)
  (when (get-buffer "*wifi*")
    (kill-buffer "*wifi*"))
  (start-process "wifi" "*wifi*" "nmcli" "dev" "wifi")
  (switch-to-buffer "*wifi*")
  (nm-mode))
