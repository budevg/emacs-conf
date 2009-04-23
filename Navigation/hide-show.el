(require 'hideshow)

(defun hide-show-hook ()
  (hs-minor-mode 1))


(add-hook 'emacs-lisp-mode-hook 'hide-show-hook)
(add-hook 'c-mode-hook 'hide-show-hook)
(add-hook 'c++-mode-hook 'hide-show-hook)
(add-hook 'cperl-mode-hook 'hide-show-hook)

(global-set-key [(control kp-add)] 'hs-show-all)
(global-set-key [(shift kp-add)] 'hs-show-block)

(global-set-key [(control kp-subtract)] 'hs-hide-all)
(global-set-key [(shift kp-subtract)] 'hs-hide-block)