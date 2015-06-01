
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)

;; Prevent the annoying beep on errors
(setq visible-bell t)

;; Annoying cursor blinking
(blink-cursor-mode -1)

(setq make-pointer-invisible t)

;; zoom in/out

(autoload 'zoom-frm-in "zoom-frm" "zoom-frm-in autoload" t)
(autoload 'zoom-frm-out "zoom-frm" "zoom-frm-out autoload" t)

(global-set-key (vector (list 'meta mouse-wheel-down-event)) 'zoom-frm-in)
(global-set-key (vector (list 'meta mouse-wheel-up-event)) 'zoom-frm-out)

(global-set-key (vector (list 'control mouse-wheel-down-event)) 'text-scale-increase)
(global-set-key (vector (list 'control mouse-wheel-up-event)) 'text-scale-decrease)

(global-unset-key (kbd "C-f"))
(global-set-key
 (kbd "C-f z")
 (defhydra hydra-zoom ()
   "zoom"
   ("<up>" text-scale-decrease "[-]" :color red)
   ("<down>" text-scale-increase "[+]" :color red)
   ("<left>" zoom-frm-out "-" :color red)
   ("<right>" zoom-frm-in "+" :color red)
   ("q" nil "cancel" :color blue)))

(global-set-key
 (kbd "C-f w")
 (defhydra hydra-windows ()
   "windows"
   ("<down>" balance-windows-area "balance windows" :color red)
   ("<left>" rename-buffer "rename buffer" :color red)
   ("<right>" rename-frame "rename frame" :color red)
   ("q" nil "cancel" :color blue)))

;; Font size
(define-key global-map [(control kp-add)] 'text-scale-increase)
(define-key global-map [(control kp-subtract)] 'text-scale-decrease)

;; full screen

(global-set-key [(super f)] 'toggle-frame-fullscreen)
