;; -*- mode: emacs-lisp -*-
(setq user-full-name "Tomas Zellerin")

(require 'org)
(require 'org-agenda)

(setq exec-path
      `(,@(mapcar 'org-link-expand-abbrev
	       '("C:/Program Files (x86)/Common Files/Oracle/Java/javapath"
		 "sw:emacs-w64-25.3-O2-with-modules/bin"
		 "sw:git/mingw64/bin/"
		 "sw:sqlite-tools-win32-x86-3220000/"
		 "sw:git/usr/bin/"
		 "sw:graphviz/bin/"
		 "sw:mupdf-1.13.0-windows/"
		 "sw:PuTTY/"
		 "sw:graphviz/bin/"
		 "sw:gnuplot/bin/"
		 "sw:rcs/"
		 "sw:R-3.5.0/bin/x64"
		 "C:/WINDOWS"
		 "C:/WINDOWS/system32"
		 "C:/WINDOWS/System32/Wbem"
		 "C:/WINDOWS/System32/WindowsPowerShell/v1.0/"))
	,nil))

(setenv "PATH" (mapconcat 'identity (butlast exec-path) ";"))


(setq org-babel-sqlite3-command "sqlite3.exe")

(setenv "GIT_SSH" (org-link-expand-abbrev "sw:putty/plink.exe"))

(setq inferior-lisp-program
      `("java" "-jar"
	,(org-link-expand-abbrev "sw:abcl-bin-1.5.0/abcl.jar")))

(setq sly-lisp-implementations nil)

(load-file (locate-user-emacs-file "windows-integration/tz-outlook.el"))
(setq org-agenda-files (mapcar 'org-link-expand-abbrev '("org:" "project:")))

(eval-after-load "org-capture"
  '(push '("O" "Outlook" entry
	  (file "weekly-review.org")
	  "%(tz-outlook-capture-item \"TODO\")\n")
	 org-capture-templates))

(eval-after-load "gnus"
  '(add-to-list 'gnus-secondary-select-methods '(nntp "news.gwene.org")))

(bind-key "<f12> o" 'tz-outlook-open-mail)
(bind-key "<f12> r" 'tz-refile-postpone)

(eval-after-load "dired"
  '(progn
     (bind-key "C-c <RET>" 'dired-w32-browser dired-mode-map)
     (bind-key "C-c o" 'dired-w32explore dired-mode-map)
     (bind-key "C-c C-o" 'dired-w32explore dired-mode-map)
     (bind-key "?" 'tz-dired-summary dired-mode-map)))

(defun tz-dired-summary ()
  (interactive)
  (dired-why)
  (message "C-c <RET> open Office file // C-c o open in Explorer"))

;;;; Screen lock interaction
(server-start)
(defun lock-windows ()
  "This function is called by Task scheduler when screen is
locked. Presumably, some interruption is in progress."
  (org-capture nil "-"))

(defun unlock-windows ()
  "This function would be called by Task scheduler when screen is
unlocked. Do nothing, let user clean up interuption.

Actually, it is disabled in my Task Scheduler at the moment.")

(use-package "w32-browser"
  :commands (dired-w32-browser))
