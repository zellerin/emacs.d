;;; Some preferred customization.
; (org-link-minor-mode)

(deftheme tz  "My preferred settings.")

(custom-theme-set-variables 'tz
 ;;
 '(add-log-always-start-new-record t)
 '(auto-insert-mode t)

 '(browse-url-browser-function 'eww-browse-url)

 '(default-input-method "czech-qwerty")
 '(dired-dwim-target t)
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.[.a-zA-Z]")

 '(eshell-visual-commands
   '( "top" "rpmreaper"))

 '(message-send-mail-function 'smtpmail-send-it)

 '(org-agenda-custom-commands
   '(("p" "Describe command here" tags-todo "TODO=\"OPEN\""
      ((org-agenda-overriding-header "Open projects")
       (org-agenda-dim-blocked-tasks nil)))))
 '(org-agenda-dim-blocked-tasks 'invisible)
 '(org-agenda-files "~/.emacs.d/local/agenda_files")
 '(org-agenda-prefix-format
   ;; show parents of todo list
   '((agenda . " %i %-12:c%?-12t% s")
     (timeline . "  % s")
     (todo .
	   " %i %-12:c %b")
     (tags .
	   " %i %-12:c %b")
     (search . " %i %-12:c")))
 '(org-agenda-files "~/.emacs.d/local/agenda_files")
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-todo-ignore-scheduled 'future)
 '(org-attach-commit nil)
 '(org-attach-method 'mv)

 '(org-babel-lisp-eval-fn 'sly-eval)
 '(org-babel-load-languages '((lisp . t) (dot . t) (emacs-lisp . t) (shell . t)))
 ;; 9.1.3 Capture templates
 '(org-capture-templates
   (append
    (tz-capture-entries
     '(("t" "TODO"		"* TODO %?\n\n")
       ("s" "small stuff"	"* TODO %? :%^{Type: |admin|followup|read}:\n\n")
       ("x" "web capture"	"* TODO %:description \n\n%:annotation %i")
       ("p" "Project"		"* PLANNED %?:project:\n\n")
       ("-" "Interruption"	"* %?\n%T\n")
       ("j" "journal item"      "* %? :journal:\n%t\n")
       ("m" "Flag mail"	        "* %:subject\n%a\n")
       ("f" "Flag place"	"* %?\n%T\n%A\n")
       ("r" "Remind person"	"* %:name\n%a\n")
       ("w" "Remind web"        "* %(tz-capture-from-eww)%^g\n%T\n\n%(tz-eww-url)\n")))
    (tz-capture-entries-clocked
     '(("T" "TODO to clocked"	"* TODO %?\n\n")))))
 '(org-capture-templates-contexts
   (append
    (tz-flag-capture-context "w" "eww-mode")
    (tz-flag-capture-context "m" "article-mode")
    (tz-flag-capture-context "m" "gnus-summary-mode")
    (tz-flag-capture-context "r" "bbdb-mode")
    `(("f" ((not-in-mode . "bbdb-mode"))))))

 '(org-enforce-todo-dependencies t)
 '(org-export-with-section-numbers nil)
 '(org-export-with-toc nil)
 '(org-global-properties '(("EFFORT_ALL" . "0:10 0:30 1:00 2:00 4:00")))
 '(org-hide-emphasis-markers t)
 '(org-id-link-to-org-use-id t)
 '(org-log-into-drawer t)
 '(org-link-abbrev-alist '(("attach" . org-attached-tag)))
 '(org-html-checkbox-type 'unicode)
 '(org-modules
   '(org-bbdb org-crypt org-docview org-gnus org-id org-info org-inlinetask org-protocol))
 '(org-hide-emphasis-markers t)
 '(org-id-link-to-org-use-id t)
 '(org-log-into-drawer t)
 '(org-refile-use-outline-path 'file)
 '(org-archive-location (concat org-directory "/archive/2019.org::datetree/* Finished tasks"))
 '(org-enforce-todo-dependencies t)
 '(org-src-window-setup 'current-window)
 '(org-todo-keywords
   '((sequence "TODO" "DONE")
     (sequence "PLAN(p)" "OPEN(o)" "|" "ONGOING" "CLOSED")
     (type "|" "WAIT(w@/@)")))
 '(org-agenda-dim-blocked-tasks 'invisible)
 '(org-refile-targets
      '((org-agenda-files :maxlevel . 2)
	(nil :maxlevel . 5)))
 ;; org babel
 '(org-confirm-babel-evaluate nil)
 '(org-babel-load-languages '((lisp . t) (dot . t) (emacs-lisp . t) (shell . t)))
 '(org-babel-lisp-eval-fn (quote sly-eval))
 '(org-src-lang-modes (cons '("dot" . graphviz-dot)
			    (customize-standard-value-of 'org-src-lang-modes)))

 ;; I need to write Czech easily.
 '(default-input-method "czech-qwerty")

 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/")))

 '(gnus-asynchronous t)
 '(gnus-secondary-select-methods
   '((nntp "news.gwene.org")))

 '(gnus-select-method '(nnml ""))
 '(gnus-use-adaptive-scoring '(word line))

 '(message-send-mail-function 'smtpmail-send-it)

 '(package-selected-packages
   '(logview simple-httpd restclient org-pdfview try poporg bbdb package-lint pic-asm workflow gnuplot dired-hacks-utils sly arduino-mode bbdb-ext dired-collapse htmlize json-mode ob-restclient no-littering magit org prodigy yasnippet xpm use-package skewer-mode redshank pretty-mode pdf-tools paredit org-bullets nameless graphviz-dot-mode google-this gitignore-mode gitconfig-mode gitattributes-mode ess elnode czech-holidays ))
 '(ring-bell-function 'ignore)

 '(savehist-mode t)
 '(sly-net-coding-system 'utf-8-unix)
 '(smtpmail-smtp-server "smtp.zoho.com")
 '(smtpmail-smtp-service 587)
 '(tz-dropbox-secret
   (funcall
    (cl-getf
     (car
      (auth-source-search :host "dropbox"))
     :secret)))

 '(file-coding-system-alist
   (cons '("\\.org\\'" . utf-8)
	 (customize-standard-value-of 'file-coding-system-alist))))


(provide-theme 'tz)
