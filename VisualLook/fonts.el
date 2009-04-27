(custom-set-faces
 '(default ((t (:foreground "white" :background "black" ))) t)
 '(diff-added ((t (:background "#003000"))))
 '(diff-hunk-header ((t (:background "blue"))))
 '(diff-removed ((t (:background "#300000"))))
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
 '(twitter-header-face ((t (:background "blue"))))
 )

(eval-after-load 'magit
  '(progn
     (custom-set-faces
      '(magit-diff-add ((t (:inherit diff-added))))
      '(magit-diff-del ((t (:inherit diff-removed))))
      '(magit-item-highlight ((t (:inherit magit-header)))))))


(custom-set-variables
 '(frame-background-mode 'dark))
