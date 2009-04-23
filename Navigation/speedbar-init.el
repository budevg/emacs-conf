
(autoload 'sr-speedbar-toggle "sr-speedbar" "sr-speedbar-toggle autoload" t)

(defun speedbar-toggle-and-switch ()
  (interactive)
  (sr-speedbar-toggle)
  (if (window-live-p sr-speedbar-window)
      (sr-speedbar-select-window)))

(global-set-key "\M-`" 'speedbar-toggle-and-switch)

