;;; small-tools.el --- Simple wrapper for commands line tools  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Tomas Zellerin

;; Author: Tomas Zellerin
;; Keywords: processes, unix, tools
;; Version: 0.9
;; Package-Requires: ((emacs "24.3"))


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Display list-style buffers with output of system commands.

;;

;;; Code:

(defvar-local small-tools-buffer-revert-commands
  nil
  "List of commands to revert content of the buffer")

(defvar small-tools-default-map (copy-keymap special-mode-map))
(bind-key "g" 'small-tools-btrfs-revert-buffer small-tools-default-map)

(defun small-tools-command-buffer (name keywords cmds &optional map force)
  "Create a buffer with list-style command output.

`NAME' will be used to create name of the buffer.
KEYWORDS is value for `font-lock-defaults'.
CMDS is either a cons of command to run and list of its parameters or
     a function to be called.
MAP, if specified, is the keymap to be used in the buffer."
  (let ((buffer (get-buffer-create (concat "*" name "*"))))
    (set-buffer buffer)
    (when (or force (zerop (buffer-size)))
      (special-mode)
      (setq font-lock-defaults
	    (list keywords))
      (font-lock-mode nil)
      (use-local-map (or map small-tools-default-map))
      (cd "/sudo::"))
    (pop-to-buffer buffer)
    (setq small-tools-buffer-revert-commands cmds)
    (small-tools-btrfs-revert-buffer)))

(defun small-tools-btrfs-revert-buffer ()
  "Update buffer to up-to-date information.

Uses shell commands or function in small-tools-buffer-revert-commands.

It is also used for the initial content preparation."
  (interactive)
  (let ((inhibit-read-only t))
    (kill-region (point-min) (point-max))
    (cl-typecase small-tools-buffer-revert-commands
      (function (funcall small-tools-buffer-revert-commands))
      (cons
       (apply 'start-file-process (buffer-name) (current-buffer) small-tools-buffer-revert-commands))))
  (goto-char 1))

(defun small-tools-line-name ()
  (save-excursion
    (move-beginning-of-line nil)
    (re-search-forward "[-a-f0-9]\\{36\\}")
    (buffer-substring-no-properties (match-beginning 0) (match-end 0))))

(define-prefix-command 'small-tools-map)

(bind-key "<f12> T" 'small-tools-map)


;;;; Btrfs
;;;###autoload
(defun small-tools-btrfs-list-subvols ()
  "List btrfs subvolumes."
  (interactive)
  (small-tools-command-buffer "BTRFS subvols"
		   '(("^ID.*"
		      (0 'font-lock-keyword-face))
		     ("^[0-9]+\\s +[0-9]+.+\t\\(<FS_TREE>\\|root\\)/.*"
		      (1 '(face italic))
		      (0 '(face default keymap
				(keymap
				 (?s . small-tools-btrfs-subvol-show))))))
		   '("btrfs" "subvol" "list" "--sort=path,rootid" "-ta" "/")))

(bind-key "s" 'small-tools-btrfs-list-subvols small-tools-map)

(defun small-tools-btrfs-subvol-show ()
  (interactive)
  (small-tools-command-buffer "BTRFS subvol show"
		   '(("^\\(ID\\) \\([0-9]+\\)"
		      (1 'font-lock-keyword-face)
		      (2 'font-lock-variable-name-face)))
		   `("btrfs" "subvol" "show" "-r" , (small-tools-line-name) "/")))

(defun small-tools-btrfs-machines ()
  (interactive)
  (small-tools-command-buffer "mount"
		   '(("SOURCE.*" 0 'bold))
		   `("machinectl")))
(bind-key "m" 'small-tools-btrfs-list-machines small-tools-map)


(defun small-tools-btrfs-list-machine-images ()
  (interactive)
  (small-tools-command-buffer "mount"
		   '(("NAME.*" 0 'font-lock-keyword-face))
		   `("machinectl" "list-images")))
(bind-key "i" 'small-tools-btrfs-list-machine-images small-tools-map)

(defun small-tools-btrfs-machine-start ()
  (interactive)
  (let ((name (small-tools-line-name)))
    (make-comint name "systemd-nspawn"
		 nil "-D" (concat "/var/lib/machines/" name))))

;;;; Network Manager

;;;###autoload
(defun small-tools-nm-list-devices ()
  "Provide interface for network manager."
  (interactive)
  (small-tools-command-buffer "nm-devices"
		   '((".*\\<connected\\>.*" 0
		      '(face bold
			     help-echo "Disconnect" pointer hand))
		     ("^DEVICE.*" (0 'font-lock-keyword-face)))
		   '("nmcli" "dev")))

(bind-key "d" 'small-tools-nm-list-devices small-tools-map)

;;;###autoload
(defun small-tools-nm-list-connections ()
  "Provide interface for network manager."
  (interactive)
  (small-tools-command-buffer "nm-connections"
		   '(("^NAME.*" (0 'font-lock-keyword-face))
		     (".*[^-\s ]\s *$" 0
		      `(face bold keymap
			     (keymap
			      (mouse-1 . small-tools-nm-connection-down)
			      (?d . small-tools-nm-connection-down))))
		     (".*--" 0
		      `(face default
			     keymap
			     (keymap
			      (mouse-1 . small-tools-nm-connection-up)
			      (?u . small-tools-nm-connection-up)))))
		   '("nmcli" "conn")))

(bind-key "c" 'small-tools-nm-list-connections small-tools-map)
(bind-key "k" 'small-tools-nm-connection-kill-maybe small-tools-map)

(defun small-tools-nm-connection-kill-maybe ()C
  (interactive)
  (when (yes-or-no-p (format "Kill %s? "  (small-tools-line-name)))
    (small-tools-nm-connection-kill)))

(cl-macrolet ((@ (name args string commands)
		 `(defun ,name ,args ,string
			 (interactive)
			 (make-process
			  :buffer "debug"
			  :name (car ,commands)
			  :command ,commands
			  :sentinel (lambda (_ _) (small-tools-btrfs-revert-buffer))))))
  (@ small-tools-nm-connection-up (&optional conn)
		       "Provide interface for network manager"
		     `("nmcli" "conn" "up" ,(or conn (small-tools-line-name))))
  (@ small-tools-nm-connection-down (&optional conn)
			 "Provide interface for network manager"
			 `("nmcli" "conn" "down" ,(or conn (small-tools-line-name))))
  (@ small-tools-nm-connection-kill (&optional conn)
			 "Provide interface for network manager"
			 `("sudo" "nmcli" "conn" "delete" ,(or conn (small-tools-line-name)))))

;;;; Mounting
;;;###autoload
(defun small-tools-mount ()
  "List mounts."
  (interactive)
  (small-tools-command-buffer "mount"
		   '(("^SOURCE.*" (0 'font-lock-keyword-face))
		     ("\\[\\([^]]*\\)\\]" 1
		      `(face bold))
		     ("[0-5][0-9]%" 0
		      `(:foreground "green")))
		   (lambda ()

		     (insert
		      (substitute-command-keys
		       "Type \\{small-tools-btrfs-subvol-map}\n\n"))
		     (narrow-to-region (point) (point-max))
		     (start-file-process (buffer-name) (current-buffer)
					 "findmnt" "-D" "-t" "ext2,ext3,ext4,btrfs"))))


(provide 'small-tools)
;;; small-tools.el ends here
