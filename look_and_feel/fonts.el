;; themes

(setq custom-safe-themes t)
(add-to-list 'custom-theme-load-path (in-emacs-d "look_and_feel/themes"))
(load-theme 'monokai t)
(custom-set-faces '(ebrowse-root-class ((t (:foreground "yellow" :weight bold)))))
