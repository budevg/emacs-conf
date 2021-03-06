(autoload 'eglot "eglot" nil t)

(if (not (fboundp 'project-root))
    (cl-defgeneric project-root (project)
      (car (project-roots project))))

(eval-after-load "eglot"
  '(progn
     (setq eglot-autoshutdown t
           eglot-stay-out-of '(eldoc flymake company imenu)
           )
     (define-key eglot-mode-map (kbd "C-/")  'xref-find-definitions)
     (define-key global-map (kbd "C-?") 'xref-pop-marker-stack)
     (define-key global-map (kbd "C-.")  'xref-find-references)
     ))
