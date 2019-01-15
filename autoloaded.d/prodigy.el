;;;###autoload
(with-eval-after-load 'prodigy
  (prodigy-define-status :id 'interrupted :face 'prodigy-yellow-face)
  (setq prodigy-services
	'((:name "wpa-supplicant"
		 :command "sudo"
		 :args ("/sbin/wpa_supplicant" "-i" "wlan" "-c" "/etc/wpa_supplicant/wpa_supplicant.conf")
		 :ready-message "CTRL-EVENT-CONNECTED"
		 :stop-signal 'sigkill
		 :on-output (lambda (&rest args)
			      (let ((output (plist-get args :output))
				    (service (plist-get args :service)))
				(when (s-matches? "CTRL-EVENT-DISCONNECTED" output)
				  (prodigy-set-status service 'interrupted)))))
	  (:name "dhclient"
		 :command "sudo"
		 :args ("dhclient" "-d" "-i" "wlan")
		 :ready-message "bound to "
		 :on-output (lambda (&rest args)
			      (let ((output (plist-get args :output))
				    (service (plist-get args :service)))
				(when (s-matches? "receive_packet failed on " output)
				  (prodigy-set-status service 'interrupted))))))))
