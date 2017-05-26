
(autoload 'python-mode "python-mode" "python-mode autoload" t)
(if (executable-find "ipython")
    (progn
      (setq py-shell-name "ipython")
      (setq py-complete-function 'py-shell-complete)))
(setq py-indent-offset 4)
(setq py-smart-indentation nil)

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

(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "epylint" (list local-file))))

(eval-after-load "flymake"
  '(progn
     (defun flymake-pylint-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
                          'flymake-create-temp-inplace))
              (local-file (file-relative-name
                           temp-file
                           (file-name-directory buffer-file-name))))
         (list "epylint" (list local-file))))
     (add-to-list 'flymake-allowed-file-name-masks
                  '("\\.py\\'" flymake-pylint-init))))
