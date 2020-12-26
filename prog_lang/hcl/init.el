(autoload 'terraform-mode "terraform-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.tf\\(vars\\)?\\'" . terraform-mode))
