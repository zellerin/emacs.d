(defun issues-table (sectname props select)
  (let ((org-trust-scanner-tags t)
	res in-recs count tag)
    (org-map-entries
     (lambda ()
       (when (equal (cdar (org-entry-properties (point) "ITEM")) sectname)
	 (setq count 0)
	 (setq tag (or (cdar (org-entry-properties (point) "KEY")) "??"))
	 (org-map-entries
	  (lambda ()
	    (incf count)
	    (dolist (p props)
	      (unless (org-entry-properties (point) p)
		(org-show-entry)
		(org-set-property p (read-string (concat p ": ")))))
	    (push (list* (format "%s%d" tag count)
			 (format
			  "[[id:%s][%s]]"
			  (org-id-get (point) t "issue")
			  (cdar (org-entry-properties (point) "ITEM")))
			 (mapcar (lambda (p)
				   (cdar (org-entry-properties (point) p)))
				 props))
		  res))
	  select 'tree))
       )
     nil 'file)
    res))


(defun org-dblock-write:issues (params)
  "Write issues block table.

PARAMS is a property list of parameters:
`:colnames' List of column names to show. It should name ID, title and used
            PROPS columns.

`:props'    List of properties to show.

`:parent'   Name of parent section for the issues. The parent section(s) can contain
            property KEY that is used to number issues.

`:select'   How to identify issue headers. Default value is \"+LEVEL=2\"."
  (insert (concat (orgtbl-to-orgtbl
		   (list* (or (plist-get params :colnames)
			      '("ID" "Název" "Dopad" "Složitost"))
			  'hline
			  (issues-table (or (plist-get params :parent) "Doporučení")
					(or (plist-get params :props)
					    '("IMPACT" "COMPLEXITY"))
					(or (plist-get params :select) "+LEVEL=2")))
		   nil)))
  (org-table-align))
