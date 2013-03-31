

(autoload 'go-mode "go-mode" nil t)

(add-to-list (quote auto-mode-alist) (cons "\\.go$" (function go-mode)))

(eval-after-load "go-mode"
  '(progn 
     (define-key go-mode-map "\C-m" 'newline-and-indent)))
