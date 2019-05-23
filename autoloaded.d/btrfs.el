(defvar-local btrfs-revert-commands
  nil
  "List of commands to revert content of the buffer")

(defun btrfs-command-buffer (name keywords cmds)
  (let ((buffer (get-buffer-create (concat "*" name "*"))))
    (set-buffer buffer)
    (unless  (< 0 (buffer-size)) ; not new
					; new
      (special-mode)
      (font-lock-mode nil)
      (font-lock-add-keywords nil keywords)
      (use-local-map btrfs-subvol-map)
      (cd "/sudo::"))
    (pop-to-buffer buffer)
    (setq btrfs-revert-commands cmds)
    (btrfs-revert-buffer)
    (goto-char 1)
    (message
     (substitute-command-keys
      "Type \\<btrfs-subvol-map>\\[btrfs-subvol-show] to quit, \\[proced-help] for help")))  )

(defun btrfs-revert-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (kill-region (point-min) (point-max))
    (apply 'start-file-process (buffer-name) (current-buffer) btrfs-revert-commands)))

;;;###autoload
(defun btrfs-list-subvols ()
  (interactive)
  (btrfs-command-buffer "BTRFS subvols"
			'(("^ID \\([0-9]+\\)"
			   (1 'font-lock-keyword-face))
			  ("path \\(\\S *\\)"
			   (1 'font-lock-variable-name-face)))
			'("btrfs" "subvol" "list" "--sort=path,rootid" "-a" "/")))


(defvar btrfs-subvol-map (copy-keymap special-mode-map))
(bind-key "s" 'btrfs-subvol-show  btrfs-subvol-map)
(bind-key "g" 'btrfs-revert-buffer  btrfs-subvol-map)

(defun btrfs-subvol-line-id ()
  (save-excursion
    (move-beginning-of-line nil)
    (re-search-forward "ID \\([0-9]+\\)")
    (buffer-substring-no-properties (match-beginning 1) (match-end 1))))

(defun btrfs-subvol-show ()
  (interactive)
  (btrfs-command-buffer "BTRFS subvol show"
			'(("^\\(ID\\) \\([0-9]+\\)"
			   (1 'font-lock-keyword-face)
			   (2 'font-lock-variable-name-face)))
			`("btrfs" "subvol" "show" "-r" , (btrfs-subvol-line-id) "/")))

(defun btrfs-mount ()
  (interactive)
  (btrfs-command-buffer "mount"
			'(("SOURCE.*" 0 'bold))
			`("findmnt" "-D" "-t" "btrfs,ext4")))
(bind-key "m" 'btrfs-mount btrfs-subvol-map)

(defun btrfs-machines ()
  (interactive)
  (btrfs-command-buffer "mount"
			'(("SOURCE.*" 0 'bold))
			`("machinectl")))

(defun btrfs-machine-images ()
  (interactive)
  (btrfs-command-buffer "mount"
			'(("NAME.*" 0 'bold))
			`("machinectl" "list-images")))


(defun btrfs-machine-name ()
  (save-excursion
    (move-beginning-of-line nil)
    (re-search-forward "\\S +")
    (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

(defun btrfs-machine-start ()
  (interactive)
  (let ((name (btrfs-machine-name)))
    (make-comint name "systemd-nspawn"
		 nil "-D" (concat "/var/lib/machines/" name))))

;;;###autoload
(defun nm-devices ()
  "Provide interface for network manager"
  (interactive)
  (btrfs-command-buffer "nm-devices"
			'((".*\\<connected\\>.*" 0
			   '(face bold
				  help-echo "Disconnect" pointer hand))
			  ("^DEVICE.*" (0 'font-lock-keyword-face)))
			'("nmcli" "dev")))

;;;###autoload
(defun nm-connections ()
  "Provide interface for network manager"
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] (lambda (x)
				(message "Disconnect: %s" (btrfs-machine-name))))
    (btrfs-command-buffer "nm-connections"
			  '((".*\\<connected\\>.*" 0
			     '(face bold
				    help-echo "Disconnect" pointer hand))
			    ("^NAME.*" (0 'font-lock-keyword-face))
			    (".*[^-\s ]\s *$" 0
			     `(face bold
				    help-echo "Disconnect" pointer hand
				    keymap
				    (keymap (mouse-1 lambda nil (interactive) (nm-connection-down (btrfs-machine-name))))
				    ))
			    (".*--" 0
			     `(face default
				    keymap
				    (keymap (mouse-1 lambda nil (interactive) (nm-connection-up (btrfs-machine-name)))))))
			  '("nmcli" "conn"))))
