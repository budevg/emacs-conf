(use-package flycheck
  :config
  (setq flycheck-python-pylint-executable "pylint")
  :bind (("C-c o" . flycheck-mode)
         ("C-c e o" . flycheck-eglot-mode)
         :map flycheck-command-map
         ("a" . flycheck-annotate-mode)
         )
  )

(use-package format-all
  :bind (("C-c p" . format-all-region-or-buffer))
  :config
  (setq-default format-all-formatters
                '(("Python" black)
                  ("Go" goimports)
                  ("Haskell" fourmolu)
                  ("C++" clang-format)
                  ("C" clang-format)
                  )))
