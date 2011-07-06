
(setq-default indent-tabs-mode nil)
(setq show-paren-style 'expression)
(setq show-paren-delay 0)
(show-paren-mode)

(require 'judge-indent)
(global-judge-indent-mode t)
(setq judge-indent-major-modes '(c-mode python-mode sh-mode))
