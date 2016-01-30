(autoload 'deft "deft" nil t)
(global-set-key (kbd "<f4>") 'deft)
(eval-after-load "deft"
  '(progn
     (setq
      deft-extension "org"
      deft-directory "~/.deft/"
      deft-use-filename-as-title t
      deft-text-mode 'org-mode)))

(global-eldoc-mode 0)
