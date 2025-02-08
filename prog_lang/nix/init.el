(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :init
  (autoload 'nix-env-load-or-reset "nix-env" nil t)
  :config
  (setq nix-indent-function 'nix-indent-line)
  (define-key nix-mode-map (kbd "C-S-p") 'nix-repl)
  (define-key nix-mode-map (kbd "C-c p") 'nix-format-buffer)
  :bind ("C-f x" . nix-env-load-or-reset)
  )
