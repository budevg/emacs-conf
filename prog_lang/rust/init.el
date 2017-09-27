(autoload 'rust-mode "rust-mode" nil t)
(autoload 'racer-find-definition "racer" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(add-to-list 'auto-mode-alist '("\\.toml\\'" . conf-mode))
(eval-after-load "rust-mode"
  '(progn
     (define-key rust-mode-map [(control /)] 'racer-find-definition)
     (define-key rust-mode-map [(control \?)] 'pop-tag-mark)
     ))
