(eval-after-load "asm-mode"
  '(progn
     (define-key asm-mode-map "TAB" 'asm-indent-line)
     (add-hook 'asm-mode-set-comment-hook
               (lambda ()
                 (set (make-local-variable 'tab-always-indent) t)))

     ))
