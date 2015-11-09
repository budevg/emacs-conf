
;grep
(autoload 'wgrep-setup "wgrep")
(add-hook 'grep-setup-hook 'wgrep-setup)
(setq grep-find-history '("find . -type f -print0 |xargs -0 -e grep -n"))
(global-set-key [(control f9)] 'grep-find)

(autoload 'moccur "color-moccur" nil t)
(global-set-key [(shift f9)] 'moccur)

;; cscope
(autoload 'cscope-index-files "xcscope" nil t)
(autoload 'cscope-find-this-symbol "xcscope" nil t)
(autoload 'cscope-find-global-definition "xcscope" nil t)
(autoload 'cscope-pop-mark "xcscope" nil t)
(autoload 'cscope-find-functions-calling-this-function "xcscope" nil t)
(autoload 'cscope-find-this-file "xcscope" nil t)

(defun cscope-update-indexer ()
  (interactive)
  (shell-command-to-string "cp /usr/bin/cscope-indexer ~/tools/bin/cscope-indexer")
  (shell-command-to-string "sed -i 's/cc|hh/cc|hh|S|go/' ~/tools/bin/cscope-indexer"))

(setq cscope-do-not-update-database t)
(define-key global-map [(meta f9)]  'cscope-find-this-symbol)
(define-key global-map [(control /)]  'cscope-find-global-definition)
(define-key global-map [(control \?)]  'cscope-pop-mark)
(define-key global-map [(control \.)]  'cscope-find-functions-calling-this-function)
(define-key global-map [(control \,)]  'cscope-find-this-file)


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
