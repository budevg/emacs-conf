
(autoload 'python-mode "python" nil t)
(autoload 'run-python "python" nil t)

(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))

(defun run-python-and-switch ()
  (interactive)
  (let ((buffer (process-buffer (run-python))))
    (switch-to-buffer-other-window buffer)))
(global-set-key [(control P)] 'run-python-and-switch)

(eval-after-load "python"
  '(progn
     (if (executable-find "ipython")
         (setq python-shell-interpreter "ipython"
               python-shell-interpreter-args "-i"))

     (setq python-indent-offset 4
           python-indent-guess-indent-offset nil)

     (add-hook
      'python-mode-hook
      (lambda ()
        (setq forward-sexp-function nil)))

     (add-hook
      'inferior-python-mode-hook
      '(lambda ()
         (set (make-local-variable 'comint-prompt-read-only) nil)
         (compilation-shell-minor-mode -1)
         ))
     ))

(autoload 'yaml-mode "yaml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

(eval-after-load "yaml-mode"
  '(progn
     (add-hook
      'yaml-mode-hook
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

(defun py-pdb-pm ()
  (interactive)
  (end-of-buffer)
  (insert "import pdb")
  (comint-send-input)
  (insert "pdb.pm()")
  (comint-send-input)
  (insert "bt")
  (comint-send-input))

(global-set-key [(control meta p)] 'py-pdb-pm)
