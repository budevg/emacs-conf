
(autoload 'python-mode "python-mode" "python-mode autoload" t)

(eval-after-load "python-mode"
  '(progn
     (if (executable-find "ipython")
         (require 'ipython))
     (add-hook 'python-mode-hook
               (lambda ()
                 (setq py-indent-offset 2)
                 (setq py-smart-indentation nil)))))

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path "~/.emacs.d/Languages/Python/pymacs-modules"))


(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
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
