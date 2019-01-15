;;;###autoload
(defun nm-wifi ()
  "Provide interface for network manager"
  (interactive)
  (when (get-buffer "*wifi*")
    (kill-buffer "*wifi*"))
  (start-process "wifi" "*wifi*" "nmcli" "dev" "wifi")
  (switch-to-buffer "*wifi*")
  (nm-mode)
  (goto-char 1))


(define-derived-mode nm-mode fundamental-mode
  "Network Manager mode"
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
