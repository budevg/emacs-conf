
(autoload 'js-mode "js" "Java script mode." t)


(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))


(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map "\C-m" 'newline-and-indent)))

(eval-after-load "js"
  '(progn
     (define-key js-mode-map "\C-m" 'newline-and-indent)
     (setq js-indent-level 2)))
