
(setq-default indent-tabs-mode nil)
(setq show-paren-style 'expression)
(setq show-paren-delay 0)
(show-paren-mode)

(require 'judge-indent)
(global-judge-indent-mode t)
(setq judge-indent-major-modes '(c-mode python-mode sh-mode))
(setq judge-indent-default-indent-width 2)
(setq judge-indent-default-tab-width 8)
(setq judge-indent-prefer-tabs-mode nil)

