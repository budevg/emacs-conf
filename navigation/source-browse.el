
;grep
(autoload 'wgrep-setup "wgrep")
(add-hook 'grep-setup-hook 'wgrep-setup)
(setq grep-find-history '("find . -type f -print0 |xargs -0 -e grep -n"))
(global-set-key [(control f9)] 'grep-find)

(autoload 'moccur "color-moccur" nil t)
(global-set-key [(shift f9)] 'moccur)

(autoload 'ggtags-find-tag-dwim "ggtags" nil t)
(autoload 'ggtags-find-definition "ggtags" nil t)
(autoload 'ggtags-find-reference "ggtags" nil t)
(autoload 'ggtags-find-tag-regexp "ggtags" nil t)
(autoload 'ggtags-find-other-symbol "ggtags" nil t)
(define-key global-map [(meta f9)]  'ggtags-find-other-symbol)
(define-key global-map [(control /)]  'ggtags-find-definition)
(define-key global-map [(control \?)]  'xref-go-back)
(define-key global-map [(control \.)]  'ggtags-find-reference)

(eval-after-load "ggtags"
  '(progn
     (setq ggtags-highlight-tag nil)
     (setq ggtags-update-on-save nil)
     (setq ggtags-global-window-height nil)
     ))

(defun ctags-build ()
  (interactive)
  (message "building project tags")
  (shell-command (concat "ctags -e -R --extra=+fq --exclude=.git -f TAGS "))
  (visit-tags-table "TAGS")
  (message "tags built successfully"))

(autoload 'etags-select-find-tag-at-point "etags-select" nil t)
(eval-after-load "etags-select"
  '(progn
     (setq etags-select-highlight-tag-after-jump nil)))
(global-set-key (kbd "M-.") 'etags-select-find-tag-at-point)
(global-set-key (kbd "C->") 'pop-tag-mark)
