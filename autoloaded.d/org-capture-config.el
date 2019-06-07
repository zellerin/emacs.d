(defun tz-capture-entry (letter name template &rest args)
  "Helper to be used in custom.el for org capture templates."
  `(,letter ,name entry
	    (file "weekly-review.org")
	    ,template
	    ,@args
	    :prepend nil :clock-in t :clock-resume t :empty-lines 1))

(defun tz-capture-entry-clocked (letter name template &rest args)
  "Helper to be used in custom.el for org capture templates."
  `(,letter ,name entry
	    (clock)
	    ,template
	    ,@args
	    :empty-lines 1))

;;;###autoload
(defun tz-capture-entries (pars)
  (mapcar (lambda (item) (apply 'tz-capture-entry item)) pars))

;;;###autoload
(defun tz-capture-entries-clocked (pars)
  (mapcar (lambda (item) (apply 'tz-capture-entry-clocked item)) pars))

;;; to be used in custom.el as
;;; '(org-capture-templates
;;;   (list
;;;    (tz-capture-entry "t" "TODO"		"* TODO %?\n\n")
;;;    (tz-capture-entry "-" "Interruption"	"* %?\n%T\n")
;;;    (tz-capture-entry "j" "journal item" "* %? :journal:\n%t\n")
;;;    (tz-capture-entry "m" "Flag mail"	"* %:subject\n%a\n")
;;;    (tz-capture-entry "f" "Flag place"	"* %?\n%T\n%A\n")
;;;    (tz-capture-entry "r" "Remind person"
;;;		       "* %:name\n%a\n")
;;;    (tz-capture-entry "w" "Remind web"  "* %(tz-capture-from-eww)%^g\n%T\n\n%(tz-eww-url)\n")))


;;;###autoload
(defun tz-flag-capture-context (letter mode)
  "Helper to be used in custom.el for org capture context."
  (let ((im (list (cons 'in-mode mode))))
    `(("f" ,letter ,im)
      (,letter ,im))))

;;; to be used in custom.el as
;;; '(org-capture-templates-contexts
;;;   (append
;;;    (tz-flag-capture-context "w" "eww-mode")
;;;    (tz-flag-capture-context "m" "article-mode")
;;;    (tz-flag-capture-context "m" "gnus-summary-mode")
;;;    (tz-flag-capture-context "r" "bbdb-mode")
;;;    `(("f" ((not-in-mode . "bbdb-mode"))))))


(defun tz-capture-from-eww ()
  (save-excursion
    (set-buffer (get-buffer "*eww*"))
    (plist-get eww-data :title)))

(defun tz-eww-url ()
  (save-excursion
    (set-buffer (get-buffer "*eww*"))
    (concat "[[" (eww-current-url) "][link]] "
	    (thing-at-point 'sentence t))))
