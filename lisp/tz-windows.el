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
	nil))

;; This is needed for magit commits to work (path to sh.exe)
(setenv "PATH" (concat
		(getenv "PATH") ";C:/Users/tzellerin/SW/git/usr/bin/"))

;; this is needed for tramp to work
(setenv "PATH" (concat
		(getenv "PATH") ";c:/Users/tzellerin/SW/PuTTY/"))

;; this is needed for git pushing
(setenv "GIT_SSH" "plink")
(setenv "PATH" (concat
		(getenv "PATH") ";C:/Users/tzellerin/SW/git/mingw64/bin/"))

(setq slime-lisp-implementations
      '((abcl ("java" "-jar"
	       "c:/Users/tzellerin/SW/abcl-bin-1.4.0/abcl-bin-1.4.0/abcl.jar"))))

(setq dashboard-items '((recents   . 5)
			(bookmarks . 5)
			(agenda    . 5)
			(projects  . 5)))


(setq org-directory "c:/Users/tzellerin/Documents/")
(setq org-agenda-files (list org-directory (concat org-directory "projects")))
