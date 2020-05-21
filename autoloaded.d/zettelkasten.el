;;;###autoload
(defun zettelkasten-card-name ()
  "Prepare name for zettelkasten card file based on name.

  Kill name so that it can be reused in the capture template.

  See [[id:424fc789-e55b-406d-93b6-83b8fc9c1f3e][refile to zettelkasten.]]"
  (let ((name  (read-string "Item: ")))
    (with-temp-buffer
      (insert name)
      (kill-region (point-min) (point-max)))
    (concat "~/org-roam/" name " "
	    (format-time-string "%Y-%m-%d")
	    ".org")))

;;;###autoload
(defun zettelkasten-insert-reference ()
  "Ask user for a refile target (that should include all zettelkasten
  methods) and insert to the current buffer link to it with description."
  (interactive)
  (let ((dest (org-refile-get-location))
	name id)
    (save-window-excursion
      (find-file (cadr dest))
      (goto-char (nth 3 dest))
      (setq id (org-id-get (point) t)
	    name (org-get-heading t t t t)))
    (org-insert-link nil (concat "id:" id) name)))

;;;###autoload
(defun zettelkasten-get-backlinks ()
  (interactive)
  (setq org-agenda-text-search-extra-files
	(directory-files "~/org-roam" t "^[^#0.].*\.org$"))
  (org-search-view nil (concat "id:" (org-id-get (point) t))))

(defcustom zettelkasten-dot-file-name
  "/tmp/foo.dot"
  "File with zettelkasten path graph."
  :group 'zettelkasten
  :type 'file)

;;;###autoload
(defun zettelkasten-display-graph ()
  (interactive)
  (let ((root (org-id-get (point) t))
	(cmd "twopi")
	(imagedirname temporary-file-directory))
    (with-temp-buffer
      (call-process "dijkstra" zettelkasten-dot-file-name t nil root)
      (call-process-region (point-min) (point-max) "gvpr" t t nil "-c"
			   "BEG_G{aset($,\"root\",\"'$root'\")}
	      N[dist<0.5]{style=\"filled\",fillcolor=\"yellow\",fontsize=\"22\"}
	      N[biblio==\"true\"]{style=\"filled\",fillcolor=\"lightblue\"}
	      N[!dist || dist>3.0]{delete(root, $)}")
      (call-process-region (point-min) (point-max) "acyclic" t t)
      (dolist (type '("png" "imap"))
	(call-process-region
	 (point-min) (point-max)
	 "twopi"
	 nil nil nil  "-Goverlap=false" "-o" (concat imagedirname "/" root "." type) "-T" type))
      (find-file (concat imagedirname "/" root ".png")))))

;; Local Variables:
;; nameless-current-name: "zettelkasten"
;; End:
