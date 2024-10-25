(use-package flycheck
  :config
  (setq flycheck-python-pylint-executable "pylint")
  :bind (("C-c o" . flycheck-mode))
  )

(use-package flycheck-eglot
  :after eglot
  :bind
  (:map eglot-mode-map
        ("C-c e o" . flycheck-eglot-mode))
  )

(use-package format-all
  :bind (("C-c p" . format-all-region-or-buffer))
  :config
  (setq format-all-formatters
        '(("Python" black)
          ("Go" goimports)
          ("Haskell" fourmolu)
          ("C++" clang-format)
          ("C" clang-format)
          )))
