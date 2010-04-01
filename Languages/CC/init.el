(setq kernel-coding-style nil)
(defun kernel-coding-style()
  (interactive)
  (if kernel-coding-style
      (progn
        (setq kernel-coding-style nil)
        (message "kernel style disabled"))
    (progn
      (setq kernel-coding-style t)
      (message "kernel style enabled"))))
      
(eval-after-load "cc-mode"
  '(progn 
     (defun my-c-mode-hook ()
       (c-set-style "linux")
       (if (not kernel-coding-style)
           (progn
             (setq c-basic-offset 2)
             (setq tab-width 2)))
       (if kernel-coding-style
           (setq indent-tabs-mode t))
       (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
     
     (defun my-c++-mode-hook ()
       (c-set-style "linux")
       (setq c-basic-offset 2)
       (setq tab-width 2)
       (font-lock-add-keywords nil
                               '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))

     (add-hook 'c-mode-hook 'my-c-mode-hook)
     (add-hook 'c++-mode-hook 'my-c++-mode-hook)


     (define-key c-mode-map "\C-m" 'newline-and-indent)
     (define-key c++-mode-map "\C-m" 'newline-and-indent)))

(autoload 'systemtap-mode "systemtap-mode")
(add-to-list 'auto-mode-alist '("\\.stp\\'" . systemtap-mode))
