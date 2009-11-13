
(autoload 'python-mode "python-mode" "python-mode autoload" t)

(eval-after-load "python-mode"
  '(progn
     (if (executable-find "ipython")
         (require 'ipython))))

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.scons$" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("SConstruct\\'" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript\\'" . python-mode))