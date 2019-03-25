; -*- mode: emacs-lisp; lexical-binding: t; -*-
;;;; Setup:
;;;; Define shodan-api-key
;;;; Use abbrev expansion to create calls.
;;;; Currently defined abbrevs:
;;;;   host/query/reverse
(require 'xml)

(defcustom shodan-api-key ""
  "API key for Shodan calls"
  :type 'string
  :group 'shodan)

(require 'restclient)

(defvar-local shodan-parsed-json
  "Parsed json of the result buffer")

;###aaautoload
(autoload 'org-babel-execute:shodan "shodan")

(org-babel-make-language-alias "shodan" "restclient")

(defvar org-babel-default-header-args:shodan
      `((:var . "key=(symbol-value 'shodan-api-key)")))

;;;###autoload
(define-derived-mode shodan-mode restclient-mode
  "Shodan mode")

(add-hook 'shodan-mode-hook 'abbrev-mode)

(define-abbrev-table 'shodan-mode-abbrev-table
  '(("host" "" shodan-host)
    ("query" "POST https://api.shodan.io/shodan/host/search?key=:key&query=")
    ("reverse" "GET https://api.shodan.io/dns/reverse?key=:key&ips=")))

(defun shodan-get-hosts (hosts)
  "Get information about list of hosts from shodan and save it to json files in current directory."
  (cl-flet ((do-next ()
		  (with-temp-buffer
		    (message "%s -> " (car hosts))
		    (insert (concat   ":key=" shodan-api-key "\n"
  "GET https://api.shodan.io/shodan/host/" (car hosts) "?key=:key"))
		    (restclient-http-send-current))))
    (setq restclient-response-loaded-hook
	  (list
	   (lambda ()
	     (unwind-protect
		 (progn
		   (message "-> %s.json" (car hosts))
		   (write-file (concat (pop hosts) ".json"))
		   (if (not hosts)
		       (setq restclient-response-loaded-hook nil)
		     (sleep-for 1 100)
		     (do-next)))))))
    (do-next)))

(define-skeleton shodan-host "Make Shodan Host query"
  "Host: "
  ":key=" shodan-api-key ?\n
  "GET https://api.shodan.io/shodan/host/" str "?key=:key"
  '(when (and nil (y-or-n-p "History")) (insert "&history=true")))

1(defun shodan-slurp (file)
  (with-current-buffer (find-file file)
    (goto-char (point-min))
    (prog1
	(json-read)
      (kill-buffer))))

(defun shodan-get-data-from-file (file path)
  (shodan-get-data (shodan-slurp file) path))

(defcustom shodan-json-path-alist
  '((http
     ("Hostname" "IP" "Port" "Reverse" "Title" "Server" "Status")
     data :all
     (:exists (:single http))
     ((:single ip_str) (:single port) (:single hostnames :all)
      (:trim http title)
      (:single http server)
      (:upto "\r\n" data)))
    (non-http
     ("Hostname" "IP" "Port" "Protocol" "Server" "Verze")
     data :all
	   (:not-exists (:single http))
	   ((:single ip_str) (:single port)
	    (:single _shodan module)
	    (:single product)
	    (:single version)))
    (ssl
     ("Hostname" "IP" "Port" "Subject CN" "Issuer CN")
     data :all
	 (:exists (:single ssl))
	 ((:single ip_str) (:single port)
	  (:single ssl cert subject CN)
	  (:single ssl cert issuer CN)))
    (org
     ("Hostname" "IP" "" "")
     data :all ((:single ip_str) (:single org) (:single isp))))
  "List of common paths to seek in Shodan jsons"
  :type '(assoc symbol t)
  :group 'shodan)

(defun shodan-get-data (base path)
  (cond ((null path) (list base))
	((atom path)
	 (message "Wrong path %s" path))
	((equal :all (car path))
	 (mapcan (lambda (el) (shodan-get-data el (cdr path)))
		 base))
	((equal :name (car path))
	 (mapcan (lambda (a) (list a)) base))
	((equal :trim (car path))
	 (let ((res (car (shodan-get-data base (cdr path)))))
	   (if (stringp res)
	       (cl-substitute ?\⋮ ?\| (with-temp-buffer (save-excursion (insert (string-trim res))) (xml-parse-string)))
	     res)))
	((equal :upto (car path))
	 (let ((res (car (shodan-get-data base (cddr path)))))
	   (if (stringp res)
	       (substring res 0 (cl-search (cadr path) res))
	     res)))
	((equal :from-to (car path))
	 (let* ((res (car (shodan-get-data base (cdddr path))))
		(start (and res (cl-search (cadr path) res))))
	   (message "%s" start)
	   (if start
	     (substring res start
			(cl-search (caddr path) res :start2 (1+ start)))
	     "")))
	((equal :single (car path))
	 (car (shodan-get-data base (cdr path))))
	((and (consp (car path))
	      (equal '= (caar path)))
	 (when (equal (car (shodan-get-data base (cddr (car path))))
		      (nth 1 (car path)))
	   (shodan-get-data base (cdr path))))
		((and (consp (car path))
	      (equal '<> (caar path)))
	 (unless (equal (car (shodan-get-data base (cddr (car path))))
		      (nth 1 (car path)))
	   (shodan-get-data base (cdr path))))
	((and (consp (car path))
	      (equal '< (caar path)))
	 (let ((val (car (shodan-get-data base (cddr (car path))))))
	   (when (<  (nth 1 (car path))
		     (if (numberp val) val
		       (string-to-number val)))
	     (shodan-get-data base (cdr path)))))
	((and (consp (car path))
	      (equal :exists (caar path)))
	 (when  (shodan-get-data base (cadr (car path)))
	   (shodan-get-data base (cdr path))))
	((and (consp (car path))
	      (equal :not-exists (caar path)))
	 (unless (shodan-get-data base (cadr (car path)))
	   (shodan-get-data base (cdr path))))
	((consp (car path))
	 (list (mapcar (lambda (item)
			 (shodan-get-data base item))
		       (car path))))
	((numberp (car path))
	 (shodan-get-data (aref base (car path))
			 (cdr path)))
	(t
	 (shodan-get-data
	  (cdr (assoc (car path) base)) (cdr path)))))

(defun domain-smaller (a b)
  (cl-labels ((greater (a b)
		     (cond
		      ((null a) nil)
		      ((null b) t)
		      ((string= (car a) (car b))
		       (greater (cdr a) (cdr b)))
		      (t (string< (car a) (car b))))))
    (greater (reverse (split-string a "\\."))
	     (reverse (split-string b "\\.")))))

(defun org-table-sort-lines-by-domain ()
  (interactive)
  (org-table-sort-lines nil ?f 'identity 'domain-smaller t))

(defun org-table-sort-lines-by-ip ()
  (interactive)
  (org-table-sort-lines nil ?f 'identity 'ip-smaller t))



(defun ip-smaller (a b)
  ; in fact, smaller or equal.
  (cl-labels ((smaller (a b)
		     (cond
		      ((null b) t)
		      ((null a) nil)
		      ((= (car a) (car b))
		       (smaller (cdr a) (cdr b)))
		      (t (<= (car a) (car b))))))
    (smaller (mapcar 'string-to-number (split-string a "\\."))
	     (mapcar 'string-to-number (split-string b "\\.")))))

(defvar names-cache nil
  "List of known hostnames and their IP addresses, or nil when undefined.")

(defun resolve-names (names)
  (let ((orig-names names) (needs-more nil))
    (setq names (cl-remove-if (lambda (a) (assoc (intern a) names-cache)) names))
    (message "%s" names)
    (when (nthcdr 30 names)
      (setq needs-more names
	    names (cl-subseq names 0 30)))
    (setq names-cache
	  (cl-remove-duplicates
	   (append names-cache
		   (when names
		     (message "Getting %s" names)
		     (save-current-buffer
		       (switch-to-buffer
			(url-retrieve-synchronously
			 (concat "https://api.shodan.io/dns/resolve?hostnames="
				 (mapconcat 'identity names ",")
				 "&key=" shodan-api-key) 10))
		       (goto-char 1)
		       (search-forward "\n\n")
		       (mapcar (lambda (a) (list (car a) (cdr a)))(json-read)))))
	   :key 'car :test 'cl-equalp))
    (if needs-more
	(resolve-names needs-more))
    (cl-remove-if-not 'cadr (mapcar (lambda (a) (assoc (intern a) names-cache)) orig-names))))


(defun domainlist-to-dnsnames (domainlist)
  "Convert domainlist in form ((domain subdomain-names) ...) into list of domains and ip addresses.."
  (mapcan (lambda (line)
	    (cons (car line)
		  (mapcar (lambda (suffix)
			    (concat suffix "." (car line)))
			  (split-string (cadr line) ","))))
	  domainlist))


;;;; (shodan-get-data my-json '(:all ip_str))
;;;; (shodan-get-data my-json '(:all (ip_str port (http title))))
;;;; (message "%s" (shodan-get-data my-json '(0 ((http title) product))))
;;;;
;;;; (message "%s" (shodan-get-data my-json '(:all ((ssl cert subject CN)
;;;; 					      (http title)
;;;; 					      (product)))))
;;;; (message "%s" (shodan-get-data my-json '(:all
;;;; 					(= 443 port)
;;;; 					((:single ssl cert subject CN)
;;;; 					      (http title)
;;;; 					      (product)))))
;;;;
;;;; (message "%s" (shodan-get-data my-json '(0
;;;; 					(= 443 port) port))
;;;; )


(defun shodan-load-host (&optional file table)
  (interactive)
  "Load current buffer (or content of a file) to postgresql json database."
  (let ((string (if file
		    (with-temp-buffer
		      (insert-file file)
		      (buffer-string))
		    (buffer-string))))
    (setq string (replace-regexp-in-string "^//.*$" "" string))
    (setq string (replace-regexp-in-string "'" "''" string))
    (setq string (replace-regexp-in-string "" "" string))
    (message  "string is \"%s\"" string)
    (org-babel-execute:sql (concat "insert into " (or table json_host_data) " (data) values ('"
				   string
				   "');")
			   '((:engine . "postgresql")))))

(defun shodan-load-search (&optional file)
  (interactive)
  "Load current buffer (or content of a file) to postgresql json database."
  (shodan-load-host file "json_search_data"))

(defun shodan-load-dir (&optional table)
  (interactive)
  (dolist (file (directory-files "." t "[0-9.]*\\.json"))
    (message "%s" file)
    (shodan-load-host file)))
