(use-package company
  :defer 4
  :config
  (setq company-backends
        '(company-capf
          company-files
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-dabbrev))
  )
