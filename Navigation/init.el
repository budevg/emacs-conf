
(autoload 'switch-file-major-mode "switch-file" nil t)

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map [(control meta o)] 'switch-file-major-mode)
     (define-key c++-mode-map [(control meta o)] 'switch-file-major-mode)))

(require 'isearch+)

;; make dired reuse the current buffer instead of opening new buffer
;; for each directory
(eval-after-load "dired"
  '(progn
     (defadvice dired-advertised-find-file (around dired-subst-directory activate)
       "Replace current buffer if file is a directory."
       (interactive)
       (let* ((orig (current-buffer))
              ;; (filename (dired-get-filename))
              (filename (dired-get-filename t t))
              (bye-p (file-directory-p filename)))
         ad-do-it
         (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
           (kill-buffer orig))))
     (define-key dired-mode-map [(control x) (control q)] 'wdired-change-to-wdired-mode)
     (require 'dired-x)
     (setq dired-omit-extensions (append dired-omit-extensions
                                         '(".ko" ".ko.cmd" ".mod.c" ".mod.o.cmd" ".o.cmd") ))))

(eval-after-load "ido"
  '(progn
     (defvar ido-execute-command-cache nil)

     (defun ido-execute-command-refresh ()
       (interactive)
       (setq ido-execute-command-cache nil))
     
     (defun ido-execute-command ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-x "
          (progn
            (unless ido-execute-command-cache
              (mapatoms
               (lambda (s)
                 (when (commandp s)
                   (setq ido-execute-command-cache
                         (cons (format "%S" s) ido-execute-command-cache))))))
            ido-execute-command-cache)))))
     (global-set-key [(control q)] 'ido-execute-command)))

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
