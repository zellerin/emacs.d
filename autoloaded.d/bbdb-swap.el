;;;###autoload
(defun bbdb-swap-names ()
  (interactive)
  "Swap first and second name"
  (let ((first (bbdb-record-firstname (bbdb-current-record)))
	(last (bbdb-record-lastname (bbdb-current-record))))
    (bbdb-record-set-field (bbdb-current-record) 'lastname first)
    (bbdb-record-set-field (bbdb-current-record) 'firstname last)
    (bbdb-redisplay-record (bbdb-current-record) t)
    (message "%s is surname now" first)))

;;;###autoload
(eval-after-load 'bbdb
  '(bind-key "<f12> /" 'bbdb-swap-names bbdb-mode-map))
