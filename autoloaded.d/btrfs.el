(defun btrfs-command-buffer (name keywords cmds)
  (let ((buffer (get-buffer-create (concat "*" name "*"))) new)
    (set-buffer buffer)
    (setq new (zerop (buffer-size)))
    (when new
      (special-mode)
      (font-lock-mode)
      (font-lock-add-keywords nil keywords)
      (use-local-map btrfs-subvol-map))
    (pop-to-buffer buffer)
    (widen)
    (kill-region (point-min) (point-max))
    (let ((default-directory "/sudo::"))
      (apply 'start-file-process name buffer cmds))
    (goto-char 1)
    (message
     (substitute-command-keys
      "Type \\<btrfs-subvol-map>\\[btrfs-subvol-show] to quit, \\[proced-help] for help")))  )

(defun btrfs-list-subvols ()
  (interactive)
  (btrfs-command-buffer "BTRFS subvols"
			'(("^\\(ID\\) \\([0-9]+\\)"
			   (1 'font-lock-keyword-face)
			   (2 'font-lock-variable-name-face)))
			'("btrfs" "subvol" "list" "/")))


(defvar btrfs-subvol-map (copy-keymap special-mode-map))
(bind-key "s" 'btrfs-subvol-show  btrfs-subvol-map)

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
			`("btrfs" "subvol" "show" "-r" , (btrfs-subvol-line-id) "/"))  )
