(use-package python
  :mode ("\\.py\\'" . python-mode)
  :init
  (defun run-python-and-switch ()
    (interactive)
    (let ((buffer (process-buffer (run-python))))
      (switch-to-buffer-other-window buffer)))
  :config
  (if (executable-find "ipython")
      (setq python-shell-interpreter "ipython"
            python-shell-interpreter-args "--simple-prompt"))

  (setq python-indent-offset 4
        python-indent-guess-indent-offset nil)

  (add-hook
   'python-mode-hook
   (lambda ()
     (setq forward-sexp-function nil)))

  (add-hook
   'inferior-python-mode-hook
   (lambda ()
     (set (make-local-variable 'comint-prompt-read-only) nil)
     (compilation-shell-minor-mode -1)
     ))
  :bind (("C-P" . run-python-and-switch))
  )

(use-package yaml-mode
  :mode ("\\.yml$" "\\.yaml$")
  :config
  (add-hook
   'yaml-mode-hook
   (lambda ()
     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

(use-package code-cells
  :mode ("\\.ipynb\\'" . code-cells-convert-ipynb)
  :custom
  (code-cells-convert-ipynb-style
   '(("jupytext" "--to" "ipynb")
     ("jupytext" "--to" "py:percent")
     (lambda () #'python-mode)
     code-cells-convert-ipynb-hook))
  :bind (:map code-cells-mode-map
         ("M-p" . code-cells-backward-cell)
         ("M-n" . code-cells-forward-cell)
         ("C-c C-c" . code-cells-eval)
         )
  )
