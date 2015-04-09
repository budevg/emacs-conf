
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(eval-after-load "go-mode"
  '(progn
     (define-key go-mode-map "\C-m" 'newline-and-indent)
     (define-key go-mode-map [(control /)] 'godef-jump)
     (define-key go-mode-map [(control \?)] 'pop-tag-mark)
     ))
