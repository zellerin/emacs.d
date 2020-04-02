;;; Some preferred customization.
; (org-link-minor-mode)

(deftheme tz-gnus  "Preferred settings for gnus")

(custom-theme-set-variables 'tz
 '(gnus-asynchronous t)
 '(gnus-secondary-select-methods  '((nntp "news.gwene.org")))
 '(gnus-select-method '(nnml ""))
 '(gnus-use-adaptive-scoring '(word line)))

(provide-theme 'tz)
