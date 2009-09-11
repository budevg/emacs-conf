
;grep
(require 'grep-edit)
(setq grep-find-history '("find . -type f -print0 |xargs -0 -e grep -n"))
(global-set-key [(control f9)] 'grep-find)

(require 'color-moccur)
(require 'moccur-edit)
(global-set-key [(shift f9)] 'moccur)

;; cscope
(autoload 'cscope-find-this-symbol "xcscope" nil t)
(autoload 'cscope-find-global-definition "xcscope" nil t)
(autoload 'cscope-pop-mark "xcscope" nil t)

(setq cscope-do-not-update-database t)
(define-key global-map [(meta f9)]  'cscope-find-this-symbol)
(define-key global-map [(control /)]  'cscope-find-global-definition)
(define-key global-map [(control \?)]  'cscope-pop-mark)
