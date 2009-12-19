(autoload 'doxymacs-mode "doxymacs" "doxymacs-mode autoload" t)
(autoload 'doxymacs-font-lock "doxymacs" "doxymacs-font-lock autoload" t)

(add-hook 'c-mode-common-hook 'doxymacs-mode)
(add-hook 'c++-mode-common-hook 'doxymacs-mode)

(defun my-doxymacs-font-lock-hook ()
  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (doxymacs-font-lock)))
(add-hook 'font-lock-mode-hook 'my-doxymacs-font-lock-hook)


(setq doxymacs-doxygen-style "Java")