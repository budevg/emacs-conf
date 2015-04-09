
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(eval-after-load "go-mode"
  '(progn
     (defun go-mode-create-imenu-index ()
       "Create and return an imenu index alist. Unlike the default
alist created by go-mode, this method creates an alist where
items follow a style that is consistent with other prog-modes."
       (let* ((patterns '(("type" "^type *\\([^ \t\n\r\f]*\\)" 1)))
              (type-index (imenu--generic-function patterns))
              (func-index))
         (save-excursion
           (goto-char (point-min))
           (while (re-search-forward go-func-meth-regexp (point-max) t)
             (let* ((var (match-string-no-properties 1))
                    (func (match-string-no-properties 2))
                    (name (if var
                              (concat (substring var 0 -1) "." func)
                            func))
                    (beg (match-beginning 0))
                    (marker (copy-marker beg))
                    (item (cons name marker)))
               (setq func-index (cons item func-index)))))
         (nconc type-index (list (cons "func" func-index)))))

     (defun go-mode-create-flat-imenu-index ()
       "Return a flat imenu index alist. See `go-mode-create-imenu-index'."
       (apply 'nconc (mapcar 'cdr (go-mode-create-imenu-index))))

     (defun go-mode-set-gopath ()
       "Set GOPATH using the VCS root of the current buffer."
       (interactive)
       (let ((go-path (read-from-minibuffer
                       "Set GOPATH: "
                       (directory-file-name default-directory))))
         (setenv "GOPATH" go-path)
         (message "GOPATH=%s" (getenv "GOPATH"))))

     (add-hook 'go-mode-hook
               (lambda ()
                 ;; use flat imenu index
                 (setq-local imenu-create-index-function
                             #'go-mode-create-flat-imenu-index)))
     (define-key go-mode-map (kbd "C-c p") 'gofmt)
     (define-key go-mode-map (kbd "C-c o") 'go-mode-set-gopath)
     (define-key go-mode-map "\C-m" 'newline-and-indent)
     (define-key go-mode-map [(control /)] 'godef-jump)
     (define-key go-mode-map [(control \?)] 'pop-tag-mark)
     ))
