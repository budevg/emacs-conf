
(autoload 'switch-file-major-mode "switch-file" nil t)

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map [(control meta o)] 'switch-file-major-mode)
     (define-key c++-mode-map [(control meta o)] 'switch-file-major-mode)))

(require 'isearch+)

(eval-after-load "dired"
  '(progn
     (defun my-dired-init ()
       ;; make dired reuse the current buffer instead of opening new buffer
       ;; for each directory
       (autoload 'dired-single-buffer "dired-single" "" t)
       (autoload 'dired-single-buffer-mouse "dired-single" "" t)
       (autoload 'dired-single-magic-buffer "dired-single" "" t)
       (autoload 'dired-single-toggle-buffer-name "dired-single" "" t)
       (define-key dired-mode-map [return] 'dired-single-buffer)
       (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
       (define-key dired-mode-map "^"
         (function
      	 (lambda nil (interactive) (dired-single-buffer "..")))))
     
     (if (boundp 'dired-mode-map)
         (my-dired-init)
       (add-hook 'dired-load-hook 'my-dired-init))
     
     (define-key dired-mode-map [(control x) (control q)] 'wdired-change-to-wdired-mode)
     (require 'dired-x)
     (setq dired-omit-extensions (append dired-omit-extensions
                                         '(".ko" ".ko.cmd" ".mod.c" ".mod.o.cmd" ".o.cmd") ))))

(eval-after-load "ido"
  '(progn
     (defadvice completing-read
       (around foo activate)
       (if (boundp 'ido-cur-list)
           ad-do-it
         (setq ad-return-value
               (ido-completing-read
                prompt
                (all-completions "" collection predicate)
                nil require-match initial-input hist def))))
     (global-set-key
      "\M-x"
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
    

  
