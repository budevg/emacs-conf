(require 'color-theme)

;;;###autoload
(defun color-theme-basic ()
  "basic color theme."
  (interactive)
  (color-theme-install
   (append
    (list 'color-theme-basic
          '((background-color . "black")
            (foreground-color . "white")
            (background-mode . 'dark))

          '(diff-added ((t (:foreground "#559944"))))
          '(diff-context (( t nil)))
          '(diff-file-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
          '(diff-function ((t (:foreground "#00bbdd"))))
          '(diff-header ((((class color) (min-colors 88) (background dark)) (:foreground "RoyalBlue1"))))
          '(diff-hunk-header ((t (:foreground "#fbde2d"))))
          '(diff-nonexistent ((t (:inherit diff-file-header :strike-through nil))))
          '(diff-refine-change ((((class color) (min-colors 88) (background dark)) (:background "#182042"))))
          '(diff-removed ((t (:foreground "#de1923"))))
          '(font-lock-comment-face ((t (:foreground "green"))))
          '(font-lock-doc-face ((t (:foreground "green2"))))
          '(font-lock-function-name-face ((t (:foreground "gold"))))
          '(font-lock-keyword-face ((t (:foreground "red" ))))
          '(font-lock-preprocessor-face ((t (:foreground "yellow"))))
          '(font-lock-string-face ((t (:foreground "cyan"))))
          '(font-lock-type-face ((t (:foreground "green3"))))
          '(font-lock-variable-name-face ((t (:foreground "aquamarine"))))
          '(font-lock-warning-face ((t (:foreground "#Ea0" :bold t))))
          '(isearch ((t (:background "cornflowerblue"))) t)
          '(show-paren-match ((t (:background "#444464"))) t)
          '(show-paren-mismatch ((t (:background "#600000"))) t)
          '(region ((t (:background "#444444"))) t)
          '(magit-header ((t)))
          '(magit-item-highlight ((t (:inherit magit-header))))
          ))))

(provide 'color-theme-basic)
