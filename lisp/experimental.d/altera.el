(defun altera-designer ()
  (interactive)
  (with-temp-buffer
    (let ((default-directory "/tmp"))
      (start-process "altera"
		     "altera" "/opt/compressed/altera-13.0sp1/quartus/bin/quartus"))))
