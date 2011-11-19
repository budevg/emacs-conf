
(defun my-comment-region ()
  (interactive)
  (let ((beg (if mark-active (region-beginning) (point-at-bol)))
	(end (if mark-active (region-end) (point-at-eol))))
    (comment-or-uncomment-region beg end)))


(setq comment-padding 0)

(global-set-key [(meta \3)] 'my-comment-region)
(global-set-key [(control \3)] 'comment-dwim)
(global-set-key [(meta \4)] 'comment-box)

