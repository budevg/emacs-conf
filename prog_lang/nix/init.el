(autoload 'nix-mode "nix-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-to-list 'auto-mode-alist '("\\.nix.in\\'" . nix-mode))

(eval-after-load "nix-mode"
  '(progn
     (setq nix-indent-function 'nix-indent-line)
     (define-key nix-mode-map (kbd "C-S-p") 'nix-repl)
     (define-key nix-mode-map (kbd "C-c p") 'nix-format-buffer)
     ))

(autoload 'nix-env-load-or-reset "nix-env" nil t)
(global-set-key (kbd "C-f x") 'nix-env-load-or-reset)
