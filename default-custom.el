;;; Some preferred customization.
; (org-link-minor-mode)

(deftheme tz
  "My preferred settings.")

(custom-theme-set-variables 'tz
 ;;
 '(auto-insert-mode t)

 ;; 9.2 Attachments
 '(org-attach-method 'mv)

 ;; 9.1.3 Capture templates
 '(org-capture-templates
   (list
    (tz-capture-entry "t" "TODO"		"* TODO %?\n\n")
    (tz-capture-entry "-" "Interruption"	"* %?\n%T\n")
    (tz-capture-entry "j" "journal item" "* %? :journal:\n%t\n")
    (tz-capture-entry "m" "Flag mail"	"* %:subject\n%a\n")
    (tz-capture-entry "f" "Flag place"	"* %?\n%T\n%A\n")
    (tz-capture-entry "r" "Remind person"
		      "* %:name\n%a\n")
    (tz-capture-entry "w" "Remind web"  "* %(tz-capture-from-eww)%^g\n%T\n\n%(tz-eww-url)\n")))
 '(org-capture-templates-contexts
   (append
    (tz-flag-capture-context "w" "eww-mode")
    (tz-flag-capture-context "m" "article-mode")
    (tz-flag-capture-context "m" "gnus-summary-mode")
    (tz-flag-capture-context "r" "bbdb-mode")
    `(("f" ((not-in-mode . "bbdb-mode"))))))

 ;; 10.3.2 [[info:org#Global%20TODO%20list][The global TODO list]]
 '(org-agenda-todo-ignore-scheduled 'future)
 '(org-scheduled-past-days 1)
 '(org-agenda-custom-commands nil)
 '(org-agenda-prefix-format
   ;; show parents of todo list
   '((agenda . " %i %-12:c%?-12t% s")
     (timeline . "  % s")
     (todo .
	   " %i %-12:c %b")
     (tags .
	   " %i %-12:c %b")
     (search . " %i %-12:c")))
 '(org-export-with-section-numbers nil)
 '(org-export-with-toc nil)

 ;; FIXME
 '(org-link-abbrev-alist (cons '("attach" . org-attached-tag)
			       (logical-names-as-org-abbrevs)))
 '(org-html-checkbox-type 'unicode)
 '(org-modules
   '(org-bbdb org-crypt org-docview org-gnus org-id org-info org-inlinetask org-protocol))
 '(org-hide-emphasis-markers t)
 '(org-id-link-to-org-use-id t)
 '(org-log-into-drawer t)
 '(org-refile-use-outline-path 'file)
 '(org-archive-location (concat org-directory "/archive/2018.org::datetree/* Finished tasks"))
 '(org-enforce-todo-dependencies t)
 '(org-src-window-setup 'current-window)
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
   '((nntp "news.gwene.org")
     (nnimap "imappro.zoho.com"
	     (nnimap-server-port 993)
	     (nnimap-inbox Inbox))
     (nnimap "imap.gmail.com"
	     (nnimap-server-port 993))))

 '(gnus-select-method '(nnml ""))
 '(gnus-use-adaptive-scoring '(word line))

 '(message-send-mail-function 'smtpmail-send-it)

 '(sly-net-coding-system 'utf-8-unix)

 '(file-coding-system-alist
   (cons '("\\.org\\'" . utf-8)
	 (customize-standard-value-of 'file-coding-system-alist))))


(provide-theme 'tz)
