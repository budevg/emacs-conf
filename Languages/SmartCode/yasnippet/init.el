
(defun load-yasnippet ()
  (setq yas-snippet-dirs "~/.emacs.d/Languages/SmartCode/yasnippet/snippets")
  (setq yas-verbosity 0)
  (require 'yasnippet)
  (yas-global-mode 1)
  )

(run-with-idle-timer 5 nil 'load-yasnippet)
