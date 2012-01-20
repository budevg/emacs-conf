
(autoload 'python-mode "python-mode" "python-mode autoload" t)
(if (executable-find "ipython")
    (progn
      (setq py-shell-name "ipython")
      (setq py-complete-function 'py-shell-complete)))
(setq py-indent-offset 2)
(setq py-smart-indentation nil)

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path (concat EMACS-CONFIG-PATH "/Languages/Python/pymacs-modules")))

(autoload 'doctest-mode "doctest-mode" "doctest-mode autoload" t)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.doctest$" . doctest-mode))
(add-to-list 'auto-mode-alist '("\\.scons$" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript\\'" . python-mode))

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(eval-after-load "yaml-mode"
  '(progn
     (add-hook 'yaml-mode-hook
               '(lambda ()
                  (define-key yaml-mode-map "\C-m" 'newline-and-indent)))))
