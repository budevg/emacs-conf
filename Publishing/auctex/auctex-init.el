
(load "auctex.el")

(eval-after-load 'latex
  '(progn
     (define-key TeX-mode-map [(meta \.)] 'TeX-complete-symbol)))