(defun replace-html-chars-region (start end)
  "Replace  “&lt;” to “<” and other chars in HTML.
This works on the current region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "&amp;" nil t) (replace-match "&" nil t))
    (goto-char (point-min))
    (while (search-forward "&lt;" nil t) (replace-match "<" nil t))
    (goto-char (point-min))
    (while (search-forward "&gt;" nil t) (replace-match ">" nil t))
    ))


(eval-after-load "nxml-mode"
  '(progn
     (defun nxml-forward-element()
       (let ((nxml-sexp-element-flag))
         (setq nxml-sexp-element-flag (not (looking-at "<!--")))
         (unless (looking-at outline-regexp)
           (condition-case nil
               (nxml-forward-balanced-item 1)
             (error nil)))))

     (add-to-list 'hs-special-modes-alist
                  '(nxml-mode
                    "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                    ""
                    "<!--" ;; won't work on its own; uses syntax table
                    (lambda (arg) (nxml-forward-element))
                    nil))

     (add-hook 'nxml-mode-hook
               (lambda ()
                 (make-local-variable 'outline-regexp)
                 (setq outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")))
     ))

(autoload 'web-mode "web-mode" nil t)
(autoload 'typescript-mode "typescript-mode" nil t)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[tj]sx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))


(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-enable-sexp-functions nil
        web-mode-enable-auto-indentation nil
        web-mode-enable-auto-quoting nil)
  (define-key web-mode-map (kbd "M-3") 'web-mode-comment-or-uncomment)
  (define-key web-mode-map (kbd "C-<down>") (lambda () (interactive) (web-mode-tag-match)))
  (define-key web-mode-map (kbd "C-<up>") (lambda () (interactive) (web-mode-tag-match)))
  (define-key web-mode-map (kbd "<backtab>")   'web-mode-fold-or-unfold)
  (set (make-local-variable 'yas--extra-modes) '(html-mode))
  )

(eval-after-load "web-mode"
  '(progn
     (add-hook 'web-mode-hook  'my-web-mode-hook)
     (setq web-mode-content-types-alist '(("jsx" . "\\.[tj]sx?\\'")))
     (add-to-list 'web-mode-comment-formats '("jsx" . "//" ))
     ))


(autoload 'emmet-mode "emmet-mode" nil t)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)

(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map "\C-m" 'newline-and-indent)))

(eval-after-load "js"
  '(progn
     (define-key js-mode-map "\C-m" 'newline-and-indent)
     (define-key js-mode-map [(meta ?.)] nil)
     (setq js-indent-level 2)))

(autoload 'restclient-mode "restclient" nil t)
