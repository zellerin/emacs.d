;;;###autoload
(defun tz-eww-toggle-images ()
  "Toggle whether images are loaded and reload the current page from cache."
  (interactive)
  (setq-local shr-inhibit-images (not shr-inhibit-images))
  (eww-reload t)
  (message "Images are now %s"
           (if shr-inhibit-images "off" "on")))

;;;###autoload
(with-eval-after-load 'eww
  (bind-key "I" 'tz-eww-toggle-images eww-mode-map)
  (setq-default shr-inhibit-images t))
