
(autoload 'python-mode "python-mode" "python-mode autoload" t)
(if (executable-find "ipython")
    (progn
      (setq py-shell-name "ipython")
      (setq py-complete-function 'py-shell-complete)))
(setq py-indent-offset 4)
(setq py-smart-indentation nil)

(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(eval-after-load "pymacs"
  '(add-to-list 'pymacs-load-path (in-emacs-d "prog_lang/python/pymacs-modules")))

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

(defun python-auto-super ()
  (interactive)
  (let (methodname classname)
    (save-excursion
      (or (py-beginning-of-def)
          (error "Enclosing def not found"))
      (or (looking-at "[ \t]*def[ \t]+\\([a-zA-Z0-9_]+\\)")
          (error "Can't determine method name"))
      (setq methodname (match-string 1))
      (or (py-beginning-of-class)
          (error "Enclosing class not found"))
      (or (looking-at "[ \t]*class[ \t]+\\([a-zA-Z0-9_]+\\)")
          (error "Can't determine class name"))
      (setq classname (match-string 1)))
    (insert (format "super(%s, self).%s()" classname methodname))
    (backward-char)))

(eval-after-load "python-mode" '(define-key python-mode-map [(control ?x) ?p ?s] 'python-auto-super))
;;(eval-after-load "python-mode" '(load-file (in-modes-d "virtualenv/virtualenv.el")))

(setq pylookup-dir (in-emacs-d "prog_lang/python/pylookup"))
(setq pylookup-program (concat pylookup-dir "/pylookup.py"))
(setq pylookup-db-file (concat pylookup-dir "/pylookup.db"))
(add-to-list 'load-path pylookup-dir)
(autoload 'pylookup-lookup "pylookup.el" nil t)
(eval-after-load "python-mode" '(define-key python-mode-map [(control ?x) ?p ?l] 'pylookup-lookup))
