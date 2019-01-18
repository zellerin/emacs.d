;;;; Setup:
;;;; Define shodan-api-key
;;;; Use abbrev expansion to create calls.
;;;; Currently defined abbrevs:
;;;;   host/query/reverse

(defcustom shodan-api-key ""
  "API key for Shodan calls"
  :type 'string)

(require 'restclient)

(defvar-local shodan-parsed-json
  "Parsed json of the result buffer")

;###aaautoload
(autoload 'org-babel-execute:shodan "shodan")

(org-babel-make-language-alias "shodan" "restclient")

(setq org-babel-default-header-args:shodan
      `((:var . "key=(symbol-value 'shodan-api-key)")))

;;;###autoload
(define-derived-mode shodan-mode restclient-mode
  "Shodan mode")

(add-hook 'shodan-mode-hook 'abbrev-mode)

(define-abbrev-table 'shodan-mode-abbrev-table
  '(("host" "" shodan-host)
    ("query" "POST https://api.shodan.io/shodan/host/search?key=:key&query=")
    ("reverse" "GET https://api.shodan.io/dns/reverse?key=:key&ips=")))

(define-skeleton shodan-host "Make Shodan Host query"
  "Host: "
  ":key " shodan-api-key ?\n
  "GET https://api.shodan.io/shodan/host/" str "?key=:key"
  '(when (y-or-n-p "History") (insert "&history=true")))

(defun shodan-slurp (file)
  (with-current-buffer (find-file file)
    (goto-char (point-min))
    (json-read)))

(defun shodan-get-data-from-file (file path)
  (shodan-get-data (shodan-slurp file) path))

(defun shodan-get-data (base path)
  (cond ((null path) (list base))
	((equal :all (car path))
	 (mapcan (lambda (el) (shodan-getdata el (cdr path)))
		 base))
	((equal :trim (car path))
	 (let ((res (car (shodan-getdata base (cdr path)))))
	   (if (stringp res) (string-trim res) res)))
	((equal :single (car path))
	 (car (shodan-getdata base (cdr path))))
	((and (consp (car path))
	      (equal '= (caar path)))
	 (when (equalp (car (shodan-getdata base (cddr (car path))))
		      (nth 1 (car path)))
	   (shodan-getdata base (cdr path))))
	((consp (car path))
	 (list (mapcar (lambda (item)
			 (shodan-getdata base item))
		       (car path))))
	((numberp (car path))
	 (shodan-getdata (aref base (car path))
			 (cdr path)))
	(t (shodan-getdata
	    (cdr (assoc (car path) base)) (cdr path)))))


;;;; (shodan-getdata my-json '(:all ip_str))
;;;; (shodan-getdata my-json '(:all (ip_str port (http title))))
;;;; (message "%s" (shodan-getdata my-json '(0 ((http title) product))))
;;;;
;;;; (message "%s" (shodan-getdata my-json '(:all ((ssl cert subject CN)
;;;; 					      (http title)
;;;; 					      (product)))))
;;;; (message "%s" (shodan-getdata my-json '(:all
;;;; 					(= 443 port)
;;;; 					((:single ssl cert subject CN)
;;;; 					      (http title)
;;;; 					      (product)))))
;;;;
;;;; (message "%s" (shodan-getdata my-json '(0
;;;; 					(= 443 port) port))
;;;; )
