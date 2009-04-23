
(require 'yasnippet)
(yas/initialize)
(run-with-idle-timer 5 nil 'yas/load-directory "~/.emacs.d/Languages/Templates/snippets")
