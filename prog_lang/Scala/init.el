(autoload 'scala-mode "scala-mode2" nil t)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))

(eval-after-load "scala-mode2"
  '(progn
     (define-key scala-mode-map "\C-m" 'newline-and-indent)
     (add-hook 'scala-mode-hook
               (lambda ()
                 ;;(scala-electric-mode)
                 (hs-org/minor-mode 1)
                 (setq scala-indent:align-parameters t)
                 (setq imenu-generic-expression
                       '(
                         ("var" "\\(var +\\)\\([^(): ]+\\)" 2)
                         ("val" "\\(val +\\)\\([^(): ]+\\)" 2)
                         ("override def" "^[ \\t]*\\(override\\) +\\(def +\\)\\([^(): ]+\\)" 3)
                         ("implicit def" "^[ \\t]*\\(implicit\\) +\\(def +\\)\\([^(): ]+\\)" 3)
                         ("def" "^[ \\t]*\\(def +\\)\\([^(): ]+\\)" 2)
                         ("trait" "\\(trait +\\)\\([^(): ]+\\)" 2)
                         ("class" "^[ \\t]*\\(class +\\)\\([^(): ]+\\)" 2)
                         ("case class" "^[ \\t]*\\(case class +\\)\\([^(): ]+\\)" 2)
                         ("object" "\\(object +\\)\\([^(): ]+\\)" 2)
                         ))))
     ))
