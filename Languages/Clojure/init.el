
(autoload 'clojure-mode "clojure-mode" "clojure-mode autoload" t nil)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
(add-hook 'clojure-mode-hook
          (lambda () (set (make-local-variable 'inferior-lisp-program) "~/tools/bin/clojure")))
(eval-after-load "clojure-mode"
  '(progn
     (define-key clojure-mode-map [(control shift p)] 'run-lisp-new)
     (define-key clojure-mode-map [(control c) (control c)] 'lisp-execute-buffer)))
