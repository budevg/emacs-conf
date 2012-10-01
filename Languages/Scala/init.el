(autoload 'scala-mode "scala-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

(eval-after-load "scala-mode"
  '(progn
     (define-key scala-mode-map "\C-m" 'newline-and-indent)
     (define-key scala-mode-map [(control tab)] nil)
     (define-key scala-mode-map [(control c)(control c)] 'scala-eval-buffer)
     (define-key scala-mode-map [f1] nil)
     ))
