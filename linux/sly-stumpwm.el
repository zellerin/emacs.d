;;;###autoload
(defun tz-run-stumpwm ()
  (interactive)
  "Start stumpwm window manager.

To be used when the emacs is first started program in X."
  (sly '("sbcl" "--eval" "(ql:quickload 'stumpwm)"
	 "--eval" "(sb-thread:make-thread #'stumpwm::stumpwm)")))
