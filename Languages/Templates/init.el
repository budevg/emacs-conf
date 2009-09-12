

(defun load-yasnippet ()
  (require 'yasnippet)
  (yas/initialize)
  (setq yas/prompt-functions '(yas/ido-prompt
                               yas/dropdown-prompt
                               yas/completing-prompt))
  (yas/load-directory "~/.emacs.d/Languages/Templates/snippets"))

(run-with-idle-timer 5 nil 'load-yasnippet)
