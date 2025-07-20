
;grep
(autoload 'wgrep-setup "wgrep")
(add-hook 'grep-setup-hook 'wgrep-setup)
(setq grep-find-history '("find . -type f -print0 |xargs -0 -e grep -n"))
(global-set-key [(control f9)] 'grep-find)

(autoload 'moccur "color-moccur" nil t)
(global-set-key [(shift f9)] 'moccur)

(use-package ggtags
  :config
  (setq ggtags-highlight-tag nil
        ggtags-update-on-save nil
        ggtags-global-window-height nil
        ggtags-global-abbreviate-filename nil)
  :bind (("M-<f9>" . ggtags-find-other-symbol)
         ("C-/" . ggtags-find-definition)
         ("C-?" . xref-go-back)
         ("C-." . ggtags-find-reference)
         :map ggtags-mode-map
         ("M-]" . nil)
         )
  )

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
