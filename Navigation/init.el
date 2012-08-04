
(autoload 'switch-file-major-mode "switch-file" nil t)

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map [(control meta o)] 'switch-file-major-mode)
     (define-key c++-mode-map [(control meta o)] 'switch-file-major-mode)))

(eval-after-load "ido"
  '(progn
     (global-set-key
      "\C-q"
      (lambda ()
        (interactive)
        (call-interactively
         (intern
          (ido-completing-read
           "M-x "
           (all-completions "" obarray 'commandp))))))
     ))

(defun rename-frame ()
  (interactive)
  (let ((new-name (read-from-minibuffer "Rename frame (to new name): ")))
    (setq frame-title-format new-name)))


(autoload 'elscreen-start "elscreen" nil t)
(eval-after-load "elscreen"
  '(progn
     (defun elscreen-get-screen-numbers-with-emphasis ()
       ""
       (interactive)
       (let ((elscreens (sort (elscreen-get-screen-list) '<))
             (emphased ""))
        
         (dolist (s elscreens)
           (setq emphased
                 (concat emphased (if (= (elscreen-get-current-screen) s)
                                      (propertize (number-to-string s)
                                                  ;;'face 'custom-variable-tag) " ")
                                                  ;;'face 'info-title-3)
                                                  'face 'font-lock-warning-face)
                                    ;;'face 'secondary-selection)
                                    (number-to-string s))
                         " ")))
         (message "screens: %s" emphased)))
    
    
     (defun elscreen-emph-prev ()
       (interactive)
       (elscreen-previous)
       (elscreen-get-screen-numbers-with-emphasis))

     (defun elscreen-emph-next ()
       (interactive)
       (elscreen-next)
       (elscreen-get-screen-numbers-with-emphasis))

     (setq elscreen-display-tab nil)

     (global-set-key (kbd "M-[") 'elscreen-emph-prev)
     (global-set-key (kbd "M-]") 'elscreen-emph-next)
     ))

(global-set-key (kbd "M-[") (lambda () (interactive) (elscreen-start)))
(global-set-key (kbd "M-]") (lambda () (interactive) (elscreen-start)))


(autoload 'nav "nav" nil t)
(global-set-key "\M-`" 'nav)

(autoload 'minimap-create "minimap" nil t)
(autoload 'minimap-kill "minimap" nil t)
(setq minimap-flag nil)
(defun minimap-show ()
  (interactive)
  (if minimap-flag
      (progn
        (minimap-kill)
        (setq minimap-flag nil))
    (progn
      (minimap-create)
      (setq minimap-flag t))))
(global-set-key [(control \`)] 'minimap-show)
    

(autoload 'ffap-file-at-point "ffap" nil t)  
(defun jump-to-file-at-point ()
  (interactive)
  (let ((file-path (ffap-file-at-point)))
    (if file-path
        (find-file-other-window file-path))))

(defun app-open-file-at-point ()
  (interactive)
  (let ((file-path (ffap-file-at-point)))
    (if file-path
        (call-process-shell-command (format "gnome-open '%s'" file-path) nil 0)
      (call-process-shell-command (format "nautilus '%s'" (expand-file-name default-directory)) nil 0))))

(define-key ctl-x-map "a" 'app-open-file-at-point)
(define-key ctl-x-map "f" 'jump-to-file-at-point)

(defun pycscope-index-files (top-directory)
  (interactive "DIndex files in directory: ")
  (call-process (concat EMACS-CONFIG-PATH "Navigation/pycscope.py") nil nil nil "-R"))
