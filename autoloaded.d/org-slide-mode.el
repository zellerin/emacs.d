;;;###autoload
(define-minor-mode org-tz-present
  "Present org mode as slides.

Bind C-x C-n and C-x C-p to move to next/previous heading, and display it clearly."
  nil " ppt"
  `((,(kbd  "C-x C-n") .  next-slide)
    (,(kbd "C-x C-p") . prev-slide))
  (cond
   (org-tz-present
    (text-scale-set 2)
    (goto-char 1)
    (next-slide)
    (variable-pitch-mode nil)
;   (set-fringe-mode 0)
;    (set-window-margins nil 45 45)
    )
   (t
    (text-scale-set 0)
    (fringe-mode 0)
    (set-window-margins nil 0 0))))

(defun switch-slide (fn amount)
    (widen)
    (right-char amount)
    (funcall fn "\^J* " nil t amount)
    (right-char amount)
    (org-narrow-to-subtree)
    (goto-char (point-min)))

(defun next-slide ()
    (interactive)
    (switch-slide 'search-forward 1))

(defun prev-slide ()
    (interactive)
    (switch-slide 'search-backward 2))

;;;###autoload
(with-eval-after-load 'org
    (bind-key "<f12> P" 'org-tz-present org-mode-map))
