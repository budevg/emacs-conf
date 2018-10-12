(autoload 'nix-mode "nix-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))
(add-to-list 'auto-mode-alist '("\\.nix.in\\'" . nix-mode))

(eval-after-load "nix-mode"
  '(progn
     (setq nix-indent-function 'nix-indent-line)
     (define-key nix-mode-map [(control P)] 'nix-repl)
     ))
