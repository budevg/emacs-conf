
(tool-bar-mode -1)
(setq inhibit-splash-screen t)

;; Prevent the annoying beep on errors
(setq visible-bell t)

(blink-cursor-mode -1)
(defun maximize-frame ()
  (set-frame-position (selected-frame) 0 26)
  (set-frame-size (selected-frame) 108 34))
(maximize-frame)
(add-hook 'window-setup-hook 'maximize-frame)

(autoload 'zoom-frm-in "zoom-frm" "zoom-frm-in autoload" t)
(autoload 'zoom-frm-out "zoom-frm" "zoom-frm-out autoload" t)

(when (boundp 'mouse-wheel-up-event)
  (global-set-key (vector (list 'control mouse-wheel-down-event)) 'zoom-frm-in))
(when (boundp 'mouse-wheel-up-event)
  (global-set-key (vector (list 'control mouse-wheel-up-event)) 'zoom-frm-out))
