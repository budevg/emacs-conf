(eval-after-load "yasnippet"
  '(progn
     (autoload 'hs-org/minor-mode "hideshow-org" nil t)

     (defun hide-show-hook ()
       (hs-org/minor-mode 1))

     (defun hide-show-hook-c ()
       (hide-show-hook)
       (hide-ifdef-mode 1)
       (local-set-key [(control kp-add)] 'show-ifdefs)
       (local-set-key [(shift kp-add)] 'show-ifdef-block)

       (local-set-key [(control kp-subtract)] 'hide-ifdefs)
       (local-set-key [(shift kp-subtract)] 'hide-ifdef-block))

     (add-hook 'emacs-lisp-mode-hook 'hide-show-hook)
     (add-hook 'c-mode-hook 'hide-show-hook-c)
     (add-hook 'c++-mode-hook 'hide-show-hook-c)
     (add-hook 'cperl-mode-hook 'hide-show-hook)
     (add-hook 'java-mode-hook 'hide-show-hook)
     (add-hook 'js-mode-hook 'hide-show-hook)
     (add-hook 'typescript-mode-hook 'hide-show-hook)
     (add-hook 'web-mode-hook 'hide-show-hook)
     (add-hook 'nxml-mode-hook 'hide-show-hook)
     (add-hook 'sh-mode-hook 'hide-show-hook)
     (add-hook 'rust-mode-hook 'hide-show-hook)
     (add-hook 'nix-mode-hook 'hide-show-hook)
     (add-hook 'go-mode-hook 'hide-show-hook)))
