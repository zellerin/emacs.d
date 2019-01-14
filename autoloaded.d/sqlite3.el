(load-library "sqlite3-api")

;;;###autoload
(defun sqlite3-get-select (file select)
  "Execute SQL select and return column names and result data.

Return cons of the column names and result data.

Interactively, insert data into tabulated buffer"
  (interactive "fFile: \nsSelect: \n")
  (sqlite3-with-db db (file sqlite-open-readonly)
		   (sqlite3-with-stmt stmt db select
				      (let ((cols
					    (cl-loop for i from 0 to (1- (sqlite3-column-count stmt))
						     collect (sqlite3-column-name stmt i)))
					    (data

					       (cl-loop while (= sqlite-row (sqlite3-step stmt))
							collect (sqlite3-fetch stmt))))
					(if (called-interactively-p 'any)
					    (tz-show-as-table cols data)
					  (cons cols data))))))

(defun tz-show-as-table (cols table)
  (switch-to-buffer "*SQL result*")
  (tabulated-list-mode)
  (setq tabulated-list-entries
	(mapcar (lambda (a) (list nil (apply 'vector
					     (mapcar  (lambda (item) (format "%s" item))
						      a))))
		table))
  (setq tabulated-list-format (apply 'vector (mapcar (lambda (a)
						       (list a 10 t))
						     cols)))
  (tabulated-list-init-header)
  (tabulated-list-print))

;;;###autoload
(defmacro sqlite3-with-db (name path &rest body)
  `(let* ((,name (sqlite3-open ,@path)))
     (unwind-protect
	 (progn
	   ,@body)
       (sqlite3-close ,name))))

(defmacro sqlite3-with-stmt (name handle cmd &rest body)
  `(let ((,name (sqlite3-prepare ,handle ,cmd)))
     (unwind-protect
	 (progn ,@body)
       (sqlite3-finalize ,name))))


;;;###autoload
(defun sqlite3-insert-table (db insert data)
  (sqlite3-with-stmt stmt db insert
		     (dolist (f data)
		       (apply #'sqlite3-bind-multi stmt f)
		       (sqlite3-step stmt)
		       (sqlite3-reset stmt))))

     ;; FIXME: symbol name has issues (e.g., varchar(255) is hard to get)


(defun sql-tabledef-cols (desc)
  (mapconcat
   (lambda (a) (concat (symbol-name (car a)) " "
		       (symbol-name (cadr a))))
   desc ", "))

(defun sql-insert-cols (desc)
  (mapconcat
   (lambda (a) "?")
   desc ", "))

(defun org-list-as-table ()
  "This allows to save org list to sqlite, e.g.,

#+CALL: write-sqlite-table(desc='((tag varchar2) (checks varchar) (content varchar)), name="changes2", data=(org-list-as-table))"
  (save-restriction
    (org-narrow-to-subtree)
    (mapcar (lambda (item) (list (nth 5 item)
				 (nth 4 item)
				 (buffer-substring-no-properties
				  (+ (length (nth 2 item))
				     (length (nth 4 item))
				     (if (nth 5 item)
					 (+ 3 (length (nth 5 item))) 0)
				     (car item))
				  (nth 6 item))
				 ))
	    (getf
	     (cadar (org-element-map (org-element-parse-buffer) '(plain-list) #'identity))
	     :structure))))
