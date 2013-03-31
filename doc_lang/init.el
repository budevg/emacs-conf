(autoload 'deft "deft" nil t)
(global-set-key (kbd "<f4>") 'deft)
(eval-after-load "deft"
  '(progn
     (setq
      deft-extension "org"
      deft-directory "~/.deft/"
      deft-text-mode 'org-mode)))
     
