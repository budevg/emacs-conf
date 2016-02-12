(autoload 'haskell-mode "haskell-mode" nil t)
(autoload 'literate-haskell-mode "haskell-mode" nil t)
(autoload 'haskell-cabal-mode "haskell-cabal" nil t)

(add-to-list 'auto-mode-alist '("\\.\\(?:[gh]s\\|hi\\)\\'" . haskell-mode))
(add-to-list 'auto-mode-alist '("\\.lhs\\'" . literate-haskell-mode))
(add-to-list 'auto-mode-alist '("\\.cabal\\'" . haskell-cabal-mode))

(autoload 'haskell-interactive-bring "haskell" nil t)
(autoload 'haskell-mode-jump-to-def-or-tag "haskell-commands" nil t)
(autoload 'haskell-process-do-type "haskell-commands" nil t)
(autoload 'haskell-process-load-or-reload "haskell" nil t)
(autoload 'haskell-doc-current-info "haskell-doc" nil t)
(autoload 'haskell-ds-create-imenu-index "haskell-decl-scan" nil t)

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map [(control P)] 'haskell-interactive-bring)
     (define-key haskell-mode-map (kbd "C-/") 'haskell-mode-jump-to-def-or-tag)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-who-calls)
     (define-key haskell-mode-map (kbd "C-?") 'pop-tag-mark)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-t") '(lambda () (interactive) (haskell-process-do-type t)))
     (custom-set-variables
      '(haskell-process-show-debug-tips nil)
      '(haskell-process-log t)
      '(haskell-process-use-presentation-mode t))
     ))

(eval-after-load "haskell-interactive-mode"
  '(progn
     (define-key haskell-interactive-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     ))

(defun haskell-who-calls (&optional prompt)
  "Grep the codebase to see who uses the symbol at point."
  (interactive "P")
  (let ((sym (if prompt
                 (read-from-minibuffer "Look for: ")
               (haskell-ident-at-point))))
    (let ((existing (get-buffer "*who-calls*")))
      (when existing
        (kill-buffer existing)))
    (let ((buffer
           (grep-find (format "cd %s && find . -name '*.hs' -exec grep -inH -e %s {} +"
                              (haskell-session-current-dir (haskell-session))
                              sym))))
      (with-current-buffer buffer
        (rename-buffer "*who-calls*")
        (switch-to-buffer-other-window buffer)))))

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
