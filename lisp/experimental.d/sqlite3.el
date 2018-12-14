(eval-after-load 'sqlite3-api
  '(progn
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

     (defun sqlite3-get-select (file select)
       "Return cons of column names and select result."
       (sqlite3-with-db db (file sqlite-open-readonly)
          (sqlite3-with-stmt stmt db select
	    (cons
	       (cl-loop for i from 0 to (1- (sqlite3-column-count stmt))
			collect (sqlite3-column-name stmt i))
	       (cl-loop while (= sqlite-row (sqlite3-step stmt))
			collect (sqlite3-fetch stmt))))))

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
	desc ", "))))
