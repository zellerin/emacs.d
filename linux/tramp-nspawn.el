;; -*- lexical-binding: t; -*-

(require 'tramp)
(require 'dbus)

;;;; Tramp interface

;; safe cleanup while experimenting, delete later
(while (equal (caar tramp-methods) "nspawn")
  (pop tramp-methods))

;;;###autoload
(defun tramp-nspawn-machines (_ignore)
  (let ((bus :system)
	(service  "org.freedesktop.machine1")
	(path "/org/freedesktop/machine1/machine")
	(interface "org.freedesktop.machine1.Machine"))
    (mapcar (lambda (node)
	      (list nil (dbus-get-property bus service
					      (concat path "/" node)
					      interface "Name")))
	    (dbus-introspect-get-node-names bus service
					    path))))

;;;###autoload
(with-eval-after-load 'tramp
  (tramp-set-completion-function
   "nspawn" '((tramp-nspawn-machines ""))))

;;;###autoload
(with-eval-after-load 'tramp
    (add-to-list 'tramp-methods
		 `("nspawn"
		   (tramp-remote-shell "/bin/sh")
		   (tramp-login-program "machinectl")
		   (tramp-login-args (("shell") ("--uid" "%u") ("%h"))))))


;;;; Tabulate
(defun dbus-props-to-vector (raw props)
  "Make a vector of specified properties in an alist.

This is used to connect dbus to tabulated mode data format."
  (apply 'vector
	 (mapcar (lambda (prop)
		   (format "%s" (cdr (assoc prop raw))))
		 props)))

(defun dbus-to-tabulated-mode (bus service path interface props)
  "Enumerate properties of interface in dbus directory and return list suitable for `tabulated-list-entries'"
  (mapcar (lambda (node)
	    (if props
		(list node
		      (dbus-props-to-vector
		       (dbus-get-all-properties bus service
						(concat path "/" node)
						interface)
		       props))
	      (dbus-get-all-properties bus service
				       (concat path "/" node)
				       interface)))
	  (dbus-introspect-get-node-names bus service path)))

(defun list-machines-parsed (&rest pars)
  "Create tabulated entries generator for dbus data"
  (lambda () (apply 'dbus-to-tabulated-mode pars)))

(cl-macrolet ((@ (fn docstring description props)
		 `(defun ,fn ()
		    (interactive)
		    ,docstring
		    (switch-to-buffer "*machine-images*")
		    (tabulated-list-mode)
		    (setq tabulated-list-entries
			  (list-machines-parsed ,@description
						',(mapcar 'car props))
			  tabulated-list-format ,(apply 'vector props))
		    (tabulated-list-init-header)
		    (tabulated-list-print))))
  (@ nspawn-tabulate-images
     "List nspawn images"
     (:system   "org.freedesktop.machine1"
		"/org/freedesktop/machine1/image"
		"org.freedesktop.machine1.Image")
     (("Name" 20 t)
       ("Path" 30 t)
       ("Type" 20 nil)))
  (@ nspawn-tabulate-machines
     "List nspawn machines"
     (:system
      "org.freedesktop.machine1"
      "/org/freedesktop/machine1/machine"
      "org.freedesktop.machine1.Machine")
     (("Name" 20 t)
      ("State" 10 t)
      ("Service" 14 t)
      ("Root" 30 nil)
      ("Net ifaces" 5 nil)))

  (@ nm-tabulate-connections
     "List nspawn machines"
     (:system   "org.freedesktop.NetworkManager"
		"/org/freedesktop/NetworkManager/Devices"
		"org.freedesktop.NetworkManager.Device")
     (("Id" 5 t)
      ("Interface" 20 t)
      ("Driver" 10 t)
      ("Ip4Address" 14 t)
      ("State" 30 nil)))

  (@ nm-tabulate-actives
  "List nspawn machines"
  (:system   "org.freedesktop.NetworkManager"
	"/org/freedesktop/NetworkManager/ActiveConnection"
	"org.freedesktop.NetworkManager.Connection.Active")
  (("Id" 20 t)
   ("Type" 10 t)
   ("State" 30 nil))))

;;; Start/stop things
(defun nspawn-image-action (action &optional name)
  "Perform action (start, stop) on the specified machine.
If machine is not specified, use name from tabulated mode or ask.

Output goes to a buffer named after machine."
  (setq name
	(or name
	    (if (eq major-mode 'tabulated-list-mode)
		(aref (tabulated-list-get-entry) 0)
	      (read-string (format "Machine to %s: " action)))))
  (make-comint name "machinectl" nil action name))

(defun nspawn-image-start (&optional name)
  (interactive)
  (nspawn-image-action "start" name))

(defun nspawn-image-stop (&optional name)
  (interactive)
  (nspawn-image-action "stop" name))
