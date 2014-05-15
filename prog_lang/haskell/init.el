(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'literate-haskell-mode "haskell-mode" nil t)
(autoload 'haskell-cabal-mode "haskell-cabal" nil t)
(defalias 'run-haskell 'switch-to-haskell)
(autoload 'switch-to-haskell "inf-haskell" nil t)

(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs\\'" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

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

(defun dot-ghci ()
  (interactive)
  (let ((config-file (expand-file-name "~/.ghc/ghci.conf"))
        (config-dir (expand-file-name "~/.ghc")))
    (make-directory config-dir t)
    (comint-send-file
     (in-emacs-d "prog_lang/haskell/ghci.conf")
     config-file)
    (chmod config-dir (string-to-number "700" 8))
    (chmod config-file (string-to-number "700" 8))
    ))
