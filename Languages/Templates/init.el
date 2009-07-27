
(require 'yasnippet)
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/dropdown-prompt
                             yas/completing-prompt))
(yas/initialize)
(run-with-idle-timer 5 nil 'yas/load-directory "~/.emacs.d/Languages/Templates/snippets")
