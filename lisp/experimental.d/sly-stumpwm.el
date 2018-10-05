(defun tz-run-stumpwm ()
  (sly '("sbcl" "--eval" "(ql:quickload 'stumpwm)"
	 "--eval" "(sb-thread:make-thread #'stumpwm::stumpwm)")))
