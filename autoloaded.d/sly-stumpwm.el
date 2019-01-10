;;;###autoload
(defun tz-run-stumpwm ()
  (interactive)
  (sly '("sbcl" "--eval" "(ql:quickload 'stumpwm)"
	 "--eval" "(sb-thread:make-thread #'stumpwm::stumpwm)")))
