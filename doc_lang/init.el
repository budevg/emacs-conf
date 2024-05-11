(autoload 'deft "deft" nil t)
(global-set-key (kbd "<f4>") 'deft)
(eval-after-load "deft"
  '(progn
     (setq
      deft-extension "org"
      deft-directory "~/.deft/"
      deft-use-filename-as-title t
      deft-text-mode 'org-mode)))

(if (fboundp 'global-eldoc-mode)
    (global-eldoc-mode 0))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (setq nov-variable-pitch nil)
  :bind (:map nov-mode-map
              ("<home>" . nil)
              ("<end>" . nil)))
