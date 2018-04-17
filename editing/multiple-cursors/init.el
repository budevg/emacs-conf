(autoload 'mc/mark-next-like-this "multiple-cursors" nil t)

(global-set-key (kbd "C-d") 'mc/mark-next-like-this)

(eval-after-load "multiple-cursors"
  '(progn
     (setq mc/always-run-for-all t)
     ))
