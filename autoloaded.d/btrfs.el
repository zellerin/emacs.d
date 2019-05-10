;;;###autoload

(defun btrfs--subvol (args)
  (switch-to-buffer "*btrfs-subvols*")
  (cd "/sudo::")
  (special-mode)
  (narrow-to-region (point-max) (point-max))
  (apply 'start-file-process "list" "*btrfs-subvols*" "btrfs" "subvol" args))

(defun btrfs-list-subvolumes ()
  (interactive)
  "List BTRFS subvolumes"
  (btrfs--subvol '("list" "-at" "/")))

(defun btrfs-subvolume-show (id)
  (interactive "nID:")
  (narrow-to-region (point-max) (point-max))
  (btrfs--subvol `("show" "-r" ,(number-to-string id) "/")))
