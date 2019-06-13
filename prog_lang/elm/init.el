(autoload 'elm-mode "elm-mode" nil t)
(autoload 'elm-format-buffer "elm-format" nil t)
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))
(eval-after-load "elm-mode"
  '(progn
     (define-key elm-mode-map (kbd "C-c C-p") 'elm-format-buffer)
     ))
