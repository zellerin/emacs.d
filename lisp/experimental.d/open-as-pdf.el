(defun tz-org-open-pdf-or-native (file _path)
  "Open pdf version of file if it exists, original file otherwise.

Intended on windows for `org-file-apps' for pptx, docx and so on."
  (if (file-readable-p (concat (file-name-sans-extension file) ".pdf"))
      (find-file (concat (file-name-sans-extension file) ".pdf"))
    (with-no-warnings (org-open-file-with-system file))))
