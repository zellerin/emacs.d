(defun tz-run-stumpwm ()
  (interactive)
  (sly '("sbcl"
	 "--load" "/home/zellerin/quicklisp/setup.lisp"
	 "--eval" "(ql:quickload 'stumpwm)"
	 "--eval" "(sb-thread:make-thread #'stumpwm::stumpwm)")))
