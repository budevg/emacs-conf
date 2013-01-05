(autoload 'haskell-mode "haskell-mode" nil t)
(defalias 'run-haskell 'switch-to-haskell)
(autoload 'switch-to-haskell "inf-haskell" nil t)

(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map [(control c) (control l)] 'nil)
     (define-key haskell-mode-map [(control c) (control b)] 'nil)
     (define-key haskell-mode-map [(control P)] 'switch-to-haskell)
     (define-key haskell-mode-map [(control c) (control c)] 'inferior-haskell-load-file)
     (setq haskell-program-name "ghci")
     (add-hook 'haskell-mode-hook (lambda ()
                                    (subword-mode +1)
                                    (turn-on-haskell-indentation)
                                    ))))
