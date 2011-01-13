(defun load-auto-complete ()
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories  "~/.emacs.d/Languages/SmartCode/auto-complete/ac-dict")
  (ac-config-default)
  (setq ac-auto-show-menu nil)
  (setq ac-cursor-color "white")
  (define-key ac-completing-map [left] 'ac-stop)
  (define-key ac-completing-map [right] 'ac-stop))


(run-with-idle-timer 6 nil 'load-auto-complete)
