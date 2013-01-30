(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-hook 'lua-mode-hook 'hs-minor-mode)
(add-hook 'lua-mode-hook
          (lambda ()
            (define-key lua-mode-map "\C-m" 'newline-and-indent)
            (define-key lua-mode-map [(control c) (control c)] 'lua-send-buffer)))
