(autoload 'flycheck-mode "flycheck" nil t)
(global-set-key [(control O)] 'flycheck-mode)
(setq flycheck-python-pylint-executable "pylint")
