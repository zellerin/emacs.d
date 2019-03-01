(define-derived-mode whois-mode fundamental-mode ()
  "Mode to display whois records"
  (setq font-lock-defaults
	`((("\n\n\\(domain\\|contact\\|nsset\\|keyset\\):\\s *\\(.*\\)"
	    (1 '(face font-lock-type-face))
	    (2 '(face font-lock-variable-name-face)))
	   ("^\\(registrar\\|nsset\\|registrant\\|tech-c\\|admin-c\\): *\\(.*\\)$"
	    (1 'font-lock-keyword-face)
	    (2 '(face font-lock-variable-name-face action whois-mode-find button (t) category default-button)))
	   )
	  nil nil ((?% . "<") (?\n . ">")))))

(defun whois-mode-find (&optional button)
  (interactive)
  (beginning-of-line)
  (re-search-forward "\\([^:]*\\):\\s *\\(.*\\)$")
  (message "%s of type %s, %s" (match-string 2) (match-string 1)))

(defun whois-do-record (type value fn)
  (beginning-of-line 2)
  (while (and (not (equal (char-after) ?\^j))
	      (re-search-forward "\\([^:]*\\):\\s *\\(.*\\)$" nil t))
    (funcall fn type value  (match-string-no-properties 1) (match-string-no-properties 2))
    (beginning-of-line 2)))

(defun whois-do-records (row-fn &optional record-fn)
  (goto-char (point-min))
  (search-forward "\n\n")
  (while (re-search-forward "^\\([^:]*\\):\\s *\\(.*\\)$" nil t)
    (when record-fn
      (funcall record-fn (match-string-no-properties 1) (match-string-no-properties 2)))
    (whois-do-record (match-string-no-properties 1) (match-string-no-properties 2) row-fn)
    (beginning-of-line 2)))

(defun test ()
  (interactive)
  (whois-do-records (lambda (a b c d)
		      (message "%s %s %s %s" a b c d))))

(load-library "sqlite3-api")
(load-library "sqlite3")

(defun whois-to-sqlite (file)
  (interactive "F")
  (sqlite3-with-db db (file sqlite-open-readwrite sqlite-open-create)
		   (sqlite3-exec db "CREATE TABLE IF NOT EXISTS whois (recordtype text, recordvalue text, itemtype text, itemvalue text)")
		   (sqlite3-with-stmt
		    stmt db "insert into whois values (?, ?, ?, ?)"
		    (sqlite3-with-stmt
		     record-stmt db "delete from whois where recordtype=? and recordvalue=?"
		     (whois-do-records
		      (lambda (a b c d)
			(sqlite3-bind-multi stmt a b c d)
			(sqlite3-step stmt)
			(sqlite3-reset stmt))
		      (lambda (a b)
			(sqlite3-bind-multi record-stmt a b)
			(sqlite3-step record-stmt)
			(sqlite3-reset record-stmt)))))))
