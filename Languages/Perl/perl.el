
(defalias 'perl-mode 'cperl-mode)
(defun my-perl-mode-hook ()
  (cperl-set-style "C++")
  (define-key cperl-mode-map "\C-m" 'newline-and-indent)
  (define-key cperl-mode-map [(control f1)] 'cperl-perldoc)
  (setq cperl-indent-level 2)
  (setq cperl-invalid-face nil)
  )



(add-hook 'cperl-mode-hook 'my-perl-mode-hook)
