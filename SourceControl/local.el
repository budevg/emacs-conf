
(defun diff-current-buffer-with-file ()
  (interactive)
  (diff-buffer-with-file (current-buffer)))

(setq diff-switches "-u")
(global-set-key [(control meta =)] 'diff-current-buffer-with-file)