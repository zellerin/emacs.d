;;;; Setup:
;;;; Define shodan-api-key
;;;; Use abbrev expansion to create calls.
;;;; Currently defined abbrevs:
;;;;   host/query/reverse

(defcustom shodan-api-key ""
  "API key for Shodan calls"
  :type 'string)

(eval-after-load "ob-restclient"
  '(progn
     (org-babel-make-language-alias "shodan" "restclient")
     (setq org-babel-default-header-args:shodan
	   `((:var . "key=(symbol-value 'shodan-api-key)")))
     (define-abbrev)))

(define-derived-mode shodan-mode restclient-mode
  "Shodan mode")

(add-hook 'shodan-mode-hook 'abbrev-mode)

(define-abbrev-table 'shodan-mode-abbrev-table
  '(("host" "" shodan-host)
    ("query" "POST https://api.shodan.io/shodan/host/search?key=:key&query=")
    ("reverse" "GET https://api.shodan.io/dns/reverse?key=:key&ips=")))

(define-skeleton shodan-host "Make Shodan Host query"
  "Host: "
  "GET https://api.shodan.io/shodan/host/" str "?key=:key"
  '(when (y-or-n-p "History") (insert "&history=true")))
