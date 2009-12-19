
(eval-after-load "cc-mode"
  '(progn 
     (defun my-cc-mode-hook ()
       (c-set-style "linux")
       (setq c-basic-offset 2)
       (setq tab-width 2)
       (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))

     (add-hook 'c-mode-hook 'my-cc-mode-hook)
     (add-hook 'c++-mode-hook 'my-cc-mode-hook)


     (define-key c-mode-map "\C-m" 'newline-and-indent)
     (define-key c++-mode-map "\C-m" 'newline-and-indent)))

(autoload 'systemtap-mode "systemtap-mode")
(add-to-list 'auto-mode-alist '("\\.stp\\'" . systemtap-mode))
