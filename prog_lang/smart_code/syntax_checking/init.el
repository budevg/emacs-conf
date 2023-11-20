(autoload 'flycheck-mode "flycheck" nil t)
(global-set-key [(control O)] 'flycheck-mode)
(setq flycheck-python-pylint-executable "pylint")

(autoload 'format-all-region-or-buffer "format-all" nil t)
(global-set-key (kbd "C-c p") 'format-all-region-or-buffer)
(setq format-all-formatters
      '(("Python" black)
        ("Go" goimports)
        ("Haskell" fourmolu)
        ("C++" clang-format)
        ("C" clang-format)
        ))
