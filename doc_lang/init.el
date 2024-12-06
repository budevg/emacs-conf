(use-package deft
  :commands (deft)
  :bind ("<f4>" . deft)
  :config
  (setq deft-extensions '("org")
        deft-directory "~/.deft/"
        deft-use-filter-string-for-filename t
        deft-recursive t
        deft-current-sort-method 'title
        deft-use-filename-as-title t
        deft-auto-save-interval 0
        deft-default-extension "org")
  )

(if (fboundp 'global-eldoc-mode)
    (global-eldoc-mode 0))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-variable-pitch nil)
  :bind (:map nov-mode-map
              ("<home>" . nil)
              ("<end>" . nil)))
