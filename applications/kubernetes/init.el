(autoload 'kubel "kubel" nil t)

(defun kubernetes ()
  (interactive)
  (require 'kubernetes)
  (kubernetes-overview)
  )

(eval-after-load "kubernetes"
  '(progn
     (setq kubernetes-redraw-frequency nil
           kubernetes-poll-frequency nil)
     (define-key kubernetes-mode-map [C-tab] nil)
     (define-key kubernetes-mode-map (kbd "C-w") #'kubernetes-copy-thing-at-point)
     ))
