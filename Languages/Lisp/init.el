
(define-key lisp-mode-shared-map  "\C-m" 'newline-and-indent)

(setq inferior-lisp-program "clisp")

(autoload 'slime "slime" nil t) 
(autoload 'slime-mode "slime" nil t)
(autoload 'slime-lisp-mode-hook "slime")

(add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)
