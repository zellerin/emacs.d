;;;###autoload
(defun logical-names-as-org-abbrevs ()
  (let (res)
    (dolist (ln logical-pathnames-names res)
      (pushnew (cons (car ln) (org-link-expand-abbrev (cdr ln)))
	       res))))

(defcustom logical-pathnames-names
  '(("org" . "~/org")
    ("emacs" . "~/.emacs"))
  "List of mapping from short name to a path.
It is currently used for org mode abbreviations, and for nice view of files in the dashboard."
  :type '(alist :key-type string :value-type directory))
