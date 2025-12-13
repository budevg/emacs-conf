(use-package deft
  :commands (deft)
  :bind ("<f4>" . deft)
  :config
  (setq deft-extensions '("org")
        deft-directory "~/.deft/"
        deft-use-filter-string-for-filename t
        deft-recursive t
        deft-current-sort-method 'title
        deft-use-filename-as-title t
        deft-auto-save-interval 0
        deft-default-extension "org")
  )

(if (fboundp 'global-eldoc-mode)
    (global-eldoc-mode 0))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-variable-pitch nil)
  :bind (:map nov-mode-map
              ("<home>" . nil)
              ("<end>" . nil)))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n c" . denote-link-after-creating)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-dired)
   ("C-c n g" . denote-grep))
  :config
  (setq denote-directory (in-emacs-d "notes"))

  ;; Automatically rename Denote buffers when opening them so that
  ;; instead of their long file name they have, for example, a literal
  ;; "[D]" followed by the file's title.  Read the doc string of
  ;; `denote-rename-buffer-format' for how to modify this.
  (denote-rename-buffer-mode 1))
