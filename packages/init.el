(autoload 'package-list-packages "package" nil t)
(autoload 'package-list-packages-no-fetch "package" nil t)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(setq el-get-recipe-path (list (in-emacs-d "packages/el-get/recipes-private")))
(autoload 'el-get-install "el-get" nil t)
(autoload 'el-get-remove "el-get" nil t)
(add-to-list 'auto-mode-alist '("\\.rcp\\'" . emacs-lisp-mode))
