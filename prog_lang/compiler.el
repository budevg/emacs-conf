
(global-set-key [f8] 'compile)
(eval-after-load "compile"
  '(progn
     (define-key compilation-mode-map (kbd "n") 'compilation-next-error)
     (define-key compilation-mode-map (kbd "p") 'compilation-previous-error)))
