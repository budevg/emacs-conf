(defun yas/advise-indent-function (function-symbol)
    (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
             ,(format
               "Try to expand a snippet before point, then call `%s' as usual"
               function-symbol)
             (let ((yas/fallback-behavior nil))
               (unless (and (interactive-p)
                            (yas/expand))
                 ad-do-it)))))

(defun yasnippet-org-mode-fix ()
  (add-hook 'org-mode-hook
          (let ((original-command (lookup-key org-mode-map [tab])))
            `(lambda ()
               (setq yas/fallback-behavior
                     '(apply ,original-command))
               (local-set-key [tab] 'yas/expand)))))
(defun load-yasnippet ()
  (require 'yasnippet)
  (yas/initialize)
  (setq yas/prompt-functions '(yas/ido-prompt
                               yas/dropdown-prompt
                               yas/completing-prompt))
  (setq yas/use-menu 'abbreviate)
  (yas/load-directory "~/.emacs.d/Languages/SmartCode/yasnippet/snippets")
  (eval-after-load 'org
    '(progn
       (yasnippet-org-mode-fix))))

(run-with-idle-timer 5 nil 'load-yasnippet)
