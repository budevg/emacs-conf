
(autoload 'iman "iman" "iman autoload" t)
(global-set-key [f1] (with-ido-completion man))
(global-set-key [(control f1)] (with-ido-completion iman))
(global-set-key [(shift f1)] 'info)
