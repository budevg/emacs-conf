(eval-when-compile (require 'subr-x))

(setq helm-dash-docsets-path (in-emacs-d ".docsets"))
(autoload 'helm-dash-at-point "helm-dash" nil t)
(autoload 'helm-dash-async-install-docset "helm-dash" nil t)
(setq helm-dash-browser-func 'eww)

(defun helm-dash-at-point-auto ()
  (interactive)
  (let* ((mode-name (capitalize
                     (string-remove-suffix
                      "-mode" (symbol-name major-mode))))
         (docset (cond
                  ((string= mode-name "Python") "Python 2")
                  (t mode-name ))))

    (setq-local helm-dash-docsets (list docset)))
  (helm-dash-at-point))

(global-set-key (kbd "M-1") 'helm-dash-at-point-auto)
