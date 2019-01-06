;;; Some preferred customization.

(custom-set-variables
 '(org-attach-method 'mv)
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
     (search . " %i %-12:c"))))
