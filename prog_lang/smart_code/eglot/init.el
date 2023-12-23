(autoload 'eglot "eglot" nil t)

(if (not (fboundp 'project-root))
    (cl-defgeneric project-root (project)
      (car (project-roots project))))

(eval-after-load "eglot"
  '(progn
     (setq eglot-autoshutdown t
           eglot-stay-out-of '(eldoc flymake company imenu)
           eglot-extend-to-xref t
           eglot-ignored-server-capabilities '(:completionProvider :inlayHintProvider)
           )

     (add-to-list 'eglot-server-programs
                  '(java-mode . ("java-language-server")))
     (add-to-list 'eglot-server-programs
                  `((js-mode typescript-mode jsx-mode) .
                    ,(eglot-alternatives
                      '(("deno" "lsp") ("typescript-language-server" "--stdio")))))

     (define-key eglot-mode-map (kbd "C-/")  'xref-find-definitions)
     (define-key eglot-mode-map (kbd "C-?") 'xref-pop-marker-stack)
     (define-key eglot-mode-map (kbd "C-.")  'xref-find-references)
     (define-key eglot-mode-map (kbd "C-c P")  'eglot-format)
     (defun eglot--post-self-insert-hook ())
     ))
