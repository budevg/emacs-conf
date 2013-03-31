(autoload 'eclim-mode "eclim" nil t)
(eval-after-load "cc-mode"
  '(progn 
     (defun my-java-mode-hook ()
       (c-set-style "java")
       (setq c-basic-offset 2)
       (if (executable-find "eclim")
           (eclim-mode 1)))

     (add-hook 'java-mode-hook 'my-java-mode-hook)

     (define-key java-mode-map "\C-m" 'newline-and-indent)))
