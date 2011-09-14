
(tool-bar-mode -1)
(setq inhibit-splash-screen t)

;; Prevent the annoying beep on errors
(setq visible-bell t)

(blink-cursor-mode -1)

;; zoom in/out

(autoload 'zoom-frm-in "zoom-frm" "zoom-frm-in autoload" t)
(autoload 'zoom-frm-out "zoom-frm" "zoom-frm-out autoload" t)

(global-set-key (vector (list 'meta mouse-wheel-down-event)) 'zoom-frm-in)
(global-set-key (vector (list 'meta mouse-wheel-up-event)) 'zoom-frm-out)

(global-set-key (vector (list 'control mouse-wheel-down-event)) 'text-scale-increase)
(global-set-key (vector (list 'control mouse-wheel-up-event)) 'text-scale-decrease)

;; Font size
(define-key global-map [(control kp-add)] 'text-scale-increase)
(define-key global-map [(control kp-subtract)] 'text-scale-decrease)

