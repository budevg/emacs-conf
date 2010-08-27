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
       (if (not kernel-coding-style)
           (progn
             (c-set-style "linux")
             (setq c-basic-offset 2)
             (setq tab-width 2))
         (progn
           (c-set-style "linux-tabs-only")
           (setq indent-tabs-mode t)))
       (font-lock-add-keywords nil
                 '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
     
     (defun my-c++-mode-hook ()
       (c-set-style "linux")
       (setq c-basic-offset 2)
       (setq tab-width 2)
       (font-lock-add-keywords nil
                               '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))

     (defun c-lineup-arglist-tabs-only (ignored)
       "Line up argument lists by tabs, not spaces"
       (let* ((anchor (c-langelem-pos c-syntactic-element))
              (column (c-langelem-2nd-pos c-syntactic-element))
              (offset (- (1+ column) anchor))
              (steps (floor offset c-basic-offset)))
         (* (max steps 1)
            c-basic-offset)))

     (add-hook 'c-mode-common-hook
               (lambda ()
                 ;; Add kernel style
                 (c-add-style
                  "linux-tabs-only"
                  '("linux" (c-offsets-alist
                             (arglist-cont-nonempty
                              c-lineup-gcc-asm-reg
                              c-lineup-arglist-tabs-only))))))

     (add-hook 'c-mode-hook 'my-c-mode-hook)
     (add-hook 'c++-mode-hook 'my-c++-mode-hook)


     (define-key c-mode-map "\C-m" 'newline-and-indent)
     (define-key c++-mode-map "\C-m" 'newline-and-indent)))

(autoload 'systemtap-mode "systemtap-mode")
(add-to-list 'auto-mode-alist '("\\.stp\\'" . systemtap-mode))
