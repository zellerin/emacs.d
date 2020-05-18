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




(put
 (defvar-local small-tools-buffer-revert-commands
   nil
   "List of commands to revert content of the buffer.

   It is marked up so that not to be deleted when major mode is set up.")
 'permanent-local t
 )


(defun small-tools-command (name cmds)
  "Create a buffer with list-style command output.

`NAME' will be used to create name of the buffer.
KEYWORDS is value for `font-lock-defaults'.
CMDS is either a cons of command to run and list of its parameters or
     a function to be called.
MAP, if specified, is the keymap to be used in the buffer,
FORCE prevents reuse of existing buffer settings
"
  (let ((buffer (get-buffer-create (concat "*" name "*"))))
    (pop-to-buffer buffer)
    (setq small-tools-buffer-revert-commands cmds)
    (small-tools-revert-buffer)))

(define-derived-mode machinectl-mode special-mode
  "machines"
  "Machines list")

(defun small-tools-command-buffer (name keywords cmds &optional map force)
  "Create a buffer with list-style command output.

`NAME' will be used to create name of the buffer.
KEYWORDS is value for `font-lock-defaults'.
CMDS is either a cons of command to run and list of its parameters or
     a function to be called.
MAP, if specified, is the keymap to be used in the buffer,
FORCE prevents reuse of existing buffer settings
"
  (let ((buffer (get-buffer-create (concat "*" name "*"))))
    (set-buffer buffer)
    (when (or force (zerop (buffer-size)))
      (special-mode)
      (setq font-lock-defaults
	    (list keywords))
      (font-lock-mode nil)
      (use-local-map (or map small-tools-default-map)))
    (pop-to-buffer buffer)
    (setq small-tools-buffer-revert-commands cmds)
    (small-tools-revert-buffer)))


(bind-key "m" 'machinectl machinectl-mode-map)
(bind-key "i" 'machinectl-images machinectl-mode-map)
(bind-key "+" 'machinectl-up machinectl-mode-map)
(bind-key "-" 'machinectl-down machinectl-mode-map)
(bind-key "RET" 'machinectl-shell machinectl-mode-map)
(bind-key "!" 'machinectl-shell-root machinectl-mode-map)
(bind-key "g" 'small-tools-revert-buffer machinectl-mode-map)

;;;###autoload
(defun machinectl ()
  (interactive)
  (small-tools-command "machinectl" '("machinectl" "-a"))
  (machinectl-mode))

(defun machinectl-images ()
  (interactive)
  (small-tools-command "machinectl" '("machinectl" "list-images"))
  (machinectl-mode))

(defun machine-name-on-line ()
  (when (equal major-mode 'machinectl-mode)
      (save-excursion
	(move-beginning-of-line nil)
	(when
	    (re-search-forward "[-a-z0-9.]+" nil t)
	  (buffer-substring-no-properties (match-beginning 0) (match-end 0))))))

(defun machinectl-up ()
  (interactive)
  (let ((buffer (current-buffer)))
    (make-process
     :buffer "debug"
     :name "machinectl-up"
     :command `("machinectl" "start" ,(machine-name-on-line))
     :sentinel (lambda (_ state)
		 (message "%s" (string-trim-right state))
		 (switch-to-buffer buffer)
		 (small-tools-revert-buffer)))))

(defun machinectl-down ()
  (interactive)
  (let ((buffer (current-buffer)))
    (make-process
     :buffer "debug"
     :name "machinectl-down"
     :command `("machinectl" "stop" ,(machine-name-on-line))
     :sentinel (lambda (_ state)
		 (message "%s" (string-trim-right state))
		 (switch-to-buffer buffer)
		 (small-tools-revert-buffer)))))

(defun machinectl-shell ()
  (interactive)
  (let ((name (machine-name-on-line)))
    (switch-to-buffer
     (make-comint name "machinectl" nil "--uid" "zellerin" "shell" name))
    (setq default-directory (concat "/nspawn:" name  ":~zellerin/"))))

(defun machinectl-term ()
  (interactive)
  (let ((name (machine-name-on-line)))
    (switch-to-buffer
     (make-term name "machinectl" nil "--uid" "zellerin" "shell" name))
    (setq default-directory (concat "/nspawn:" name  ":~zellerin/"))))


(defun machinectl-shell-root ()
  (interactive)
  (let ((name (machine-name-on-line)))
    (switch-to-buffer
     (make-comint name "machinectl" nil "shell" name))
    (setq comint-input-ignoredups t)
    (setq default-directory (concat "/nspawn:" name  ":~root/"))))


(defvar small-tools-default-map (copy-keymap special-mode-map))
(bind-key "g" 'small-tools-revert-buffer small-tools-default-map)


(defun small-tools-revert-buffer ()
  "Update buffer to up-to-date information.

Uses shell commands or function in small-tools-buffer-revert-commands.

It is also used for the initial content preparation."
  (interactive)
  (let ((inhibit-read-only t))
    (kill-region (point-min) (point-max))
    (cl-typecase small-tools-buffer-revert-commands
      (function (funcall small-tools-buffer-revert-commands))
      (cons
       (let ((default-directory "/sudo::"))
	 (apply 'start-file-process (buffer-name) (current-buffer) small-tools-buffer-revert-commands)))))
  (goto-char 1))

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

(defun small-tools--run-command (command &rest args)
  (switch-to-buffer (generate-new-buffer command))
  (let ((default-directory "/sudo::~/"))
    (apply 'start-file-process command (current-buffer) command args)))

(defun small-tools-btrfs-machine-start (&optional machine)
  (interactive)
  (let ((name (or machine (read-file-name "Machine: " "/var/lib/machines/" "scratch" t))))
    (small-tools--run-command "machinectl" "start" (file-name-base name))))

;;;; Network Manager

(bind-key "d" 'small-tools-nm-list-devices small-tools-map)

;;;###autoload


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
