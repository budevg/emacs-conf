
;grep
(require 'grep-edit)
(setq grep-find-history '("find . -type f -print0 |xargs -0 -e grep -n"))
(global-set-key [(control f9)] 'grep-find)

(require 'color-moccur)
(require 'moccur-edit)
(global-set-key [(shift f9)] 'moccur)

;; cscope
(autoload 'cscope-find-this-symbol "xcscope" "cscope-find-this-symbol autoload" t)
(autoload 'cscope-find-global-definition "xcscope" "cscope-find-global-definition autoload" t)
(autoload 'cscope-pop-mark "xcscope" "cscope-pop-mark autoload" t)

(setq cscope-do-not-update-database t)
(define-key global-map [(meta f9)]  'cscope-find-this-symbol)
(define-key global-map [(control /)]  'cscope-find-global-definition)
(define-key global-map [(control \?)]  'cscope-pop-mark)
