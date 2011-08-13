
(autoload 'clojure-mode "clojure-mode" "clojure-mode autoload" t nil)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-hook 'clojure-mode-hook
          (lambda () (set (make-local-variable 'inferior-lisp-program) "~/tools/bin/clojure")))