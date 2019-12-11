(autoload 'flycheck-mode "flycheck" nil t)
(global-set-key [(control O)] 'flycheck-mode)
(setq flycheck-python-pylint-executable "pylint")

(autoload 'format-all-buffer "format-all" nil t)
(global-set-key (kbd "C-c p") 'format-all-buffer)
