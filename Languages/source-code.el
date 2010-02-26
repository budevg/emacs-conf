
(setq-default indent-tabs-mode nil)
(autoload 'dtrt-indent-adapt "dtrt-indent" "Adapt to foreign indentation offsets" t)
(global-set-key [(control x) t] 'dtrt-indent-adapt)

(setq show-paren-style 'expression)
(setq show-paren-delay 0)
(show-paren-mode)

