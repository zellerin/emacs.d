(defcustom wine-programs nil
  "List of programs runnable by Wine"
  :type '(repeat (list (string :tag "Name") directory (string :tag "Program")))
  :group 'experimental)

(defcustom opt-programs nil
  "List of GUI programs to run in specific directory"
  :type '(repeat (list (string :tag "Name") directory
		       (string :tag "Program")
		       (repeat string)))
  :group 'experimental)

(defun all--programs (base wine)
  (append base
	  (mapcar (lambda (p)
		    `(,(car p)  ,(cadr p)
		      "/usr/bin/wine32" ,(nthcdr 2 p)))
			  wine)))

;;;###autoload
(defun run-gui-program ()
  "Run a Wine program picked from a list based on `opt-programs' and `wine-programs'."
  (interactive)
  (let* ((all-programs (all--programs opt-programs wine-programs))
	 (program-name (completing-read "Program: "
					all-programs
					nil t))
	 (program (assoc program-name all-programs))
	 (default-directory (nth 1 program))
	 (prog-and-args (cddr program)))
    (apply 'make-comint program-name (car prog-and-args) nil (cadr prog-and-args))))

;;;###autoload
(bind-key "<f12> W" 'run-gui-program)
