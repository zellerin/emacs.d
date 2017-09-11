;; -*- mode: emacs-lisp -*-
(setq user-full-name "Tomas Zellerin")

(setq exec-path
      '("c:/ProgramData/Oracle/Java/javapath"
	"C:/Windows/system32"
	"C:/Windows"
	"C:/Windows/System32/Wbem"
	"C:/Windows/System32/WindowsPowerShell/v1.0/"
	"c:/Users/tzellerin/SW/PuTTY/"
	"C:/Users/tzellerin/SW/git/mingw64/bin/"
	"C:/Users/tzellerin/SW/git/usr/bin/"
	"C:/Users/tzellerin/SW/emacs-w64-25.1-O2-with-modules/bin"
	nil))

(setq org-directory "C:/Users/tzellerin/Documents/Projects/")

;; This is needed for magit commits to work (path to sh.exe)
(setenv "PATH" (concat
		;; I want find.exe from here to take precedence
		"C:/Users/tzellerin/SW/git/usr/bin/;" (getenv "PATH")))

;; this is needed for tramp to work
(setenv "PATH" (concat
		(getenv "PATH") ";c:/Users/tzellerin/SW/PuTTY/"))

(setenv "PATH" (concat
		(getenv "PATH")
		";C:/Users/tzellerin/SW/emacs-w64-25.1-O2-with-modules/bin"))

;; this is needed for git pushing
(setenv "GIT_SSH" "plink")
(setenv "PATH" (concat
		(getenv "PATH") ";C:/Users/tzellerin/SW/git/mingw64/bin/"))

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
	  "%(tz-outlook-capture-item)")
	org-capture-templates))

(setq experimental-logical-names
      '(("docs" . "c:/Users/tzellerin/documents/")
	("conf" . "c:/Users/tzellerin/configs/")))
