
(eval-after-load "ruby-mode"
  '(progn 
      (require 'inf-ruby)
      (require 'ruby-electric)
      
      
      (add-hook 'ruby-mode-hook
                '(lambda ()
                   (inf-ruby-keys)
                   (ruby-electric-mode)
                   ))

      (define-key ruby-mode-map "\C-m" 'newline-and-indent)))




