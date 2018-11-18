(defun mplayer-play-link ()
  (interactive)
  (apply 'make-comint "mplayer" "mplayer" nil (eww-links-at-point)))
