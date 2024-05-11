(use-package eglot
  :commands (eglot)
  :config
  (setq eglot-autoshutdown t
        eglot-stay-out-of '(eldoc flymake company imenu)
        eglot-extend-to-xref t
        eglot-events-buffer-size 0
        eglot-ignored-server-capabilities '(:completionProvider :inlayHintProvider)
        )
  (add-to-list 'eglot-server-programs
               '(java-mode . ("java-language-server")))
  (add-to-list 'eglot-server-programs
               `((js-mode typescript-mode jsx-mode) .
                 ,(eglot-alternatives
                   '(("deno" "lsp") ("typescript-language-server" "--stdio")))))
  (defun eglot--post-self-insert-hook ())
  :bind
  (:map eglot-mode-map
        ("C-/" .  'xref-find-definitions)
        ("C-?" . 'xref-go-back)
        ("C-." .  'xref-find-references)
        ("C-c P" .  'eglot-format))
  )

(if (not (fboundp 'project-root))
    (cl-defgeneric project-root (project)
      (car (project-roots project))))
