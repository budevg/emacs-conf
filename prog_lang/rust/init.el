(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t)
  :mode ("\\.rs\\'" . rust-mode)
  :bind (:map rust-mode-map
         ("C-c p" . rust-format-buffer)))

(use-package conf-mode
  :mode ("\\.toml\\'" . conf-mode))
