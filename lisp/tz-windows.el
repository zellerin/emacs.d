;; -*- mode: emacs-lisp -*-
(setq user-full-name "Tomas Zellerin")

(setq exec-path
      '("C:/ProgramData/Oracle/Java/javapath" "C:/Users/tzellerin/SW/emacs-w64-25.3-O2-with-modules/bin" "C:/Users/tzellerin/SW/git/mingw64/bin/" "C:/Users/tzellerin/SW/git/usr/bin/" "c:/Users/tzellerin/SW/graphviz/bin/" "c:/Users/tzellerin/SW/mupdf-1.11-windows/" "c:/Users/tzellerin/SW/PuTTY/" "c:/Users/tzellerin/SW/PuTTY/c:/Users/tzellerin/SW/graphviz/bin/" "c:/Users/tzellerin/SW/R-3.4.2/R/bin/x64" "C:/WINDOWS" "C:/WINDOWS/system32" "C:/WINDOWS/System32/Wbem" "C:/WINDOWS/System32/WindowsPowerShell/v1.0/" nil))
(setenv "PATH" (mapconcat 'identity (butlast exec-path) ";"))

(setenv "GIT_SSH" "C:/Users/tzellerin/SW/putty/plink.exe")

(setq slime-lisp-implementations
      '((abcl ("java" "-jar"
	       "c:/Users/tzellerin/SW/abcl-bin-1.4.0/abcl-bin-1.4.0/abcl.jar"))))

(setq dashboard-items '((tips . 5)
			(agenda    . 5)
			(projects  . 5)
			(recents   . 10)
			(bookmarks . 5)))

(setq org-directory "c:/Users/tzellerin/Documents/")
(setq org-agenda-files (list org-directory (concat org-directory "projects")))

(defun tz-capture-from-eww ()
  (save-excursion
    (set-buffer (get-buffer "*eww*"))
    (plist-get eww-data :title)))

(defun tz-eww-url ()
  (save-excursion
    (set-buffer (get-buffer "*eww*"))
    (concat "[[" (eww-current-url) "]["
	    (thing-at-point 'sentence t)
	    "]]")))

(eval-after-load "org"
  '(load-file (locate-user-emacs-file
	      "windows-integration/tz-outlook.el")))

(eval-after-load "org-capture"
  '(push '("O" "Outlook" entry
	  (file "weekly-review.org")
	  "%(tz-outlook-capture-item \"TODO\")\n")
	org-capture-templates))

(setq experimental-logical-names
      '(("docs" . "c:/Users/tzellerin/documents/")
	("conf" . "c:/Users/tzellerin/configs/")))

(defun tz-refile-postpone ()
  "Refile (from daily) to weekly topics. Presumably, the task was
finished, but needs to stay in weekly file for
tracking (especially time tracking) purposes. "
  (interactive)
  (org-todo "DONE")
  (org-refile nil  nil (list "Finished" "weekly-review.org"
			     (buffer-size (find-buffer-visiting "c:/Users/tzellerin/Documents/weekly-review.org")))))

(use-package "hydra"
  :config
  (defhydra hydra-work (global-map "<f12>") "Work tasks"
    ("O" tz-outlook-open-mail "Open mail")
    ("-" tz-refile-postpone "Close and refile for tracking")))

(eval-after-load "dired"
  '(bind-key "<RET>" 'dired-w32-browser dired-mode-map))
