
(defun transpose-buffers (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))


(global-set-key [(control tab)] 'other-window)
(global-set-key [(meta up)] 'windmove-up)
(global-set-key [(meta down)] 'windmove-down)
(global-set-key [(pause)] 'kill-this-buffer)
(global-set-key [(control pause)] 'delete-frame)

(ido-mode t)
(icomplete-mode t)
(global-set-key [(control \;)] 'ido-switch-buffer)
(global-set-key [(control \])] 'ido-find-file)


(global-set-key [(control right)] 'forward-sexp)
(global-set-key [(control left)]  'backward-sexp)

(global-set-key [(meta pause)] 'delete-window)
(global-set-key [f5]  'delete-other-windows)
(global-set-key [(control f5)]  'new-frame)
(global-set-key [(meta f5)]  'transpose-buffers)

(fset 'yes-or-no-p 'y-or-n-p)


;; sticky windows
(defun change-window-sticky ()
  (interactive)
  (set-window-dedicated-p (selected-window) (not (window-dedicated-p (selected-window)))))

(global-set-key [(f6)] 'change-window-sticky)

(autoload 'ibuffer-other-window "ibuffer" nil t)
(eval-after-load "ibuffer"
  '(progn
     (setq ibuffer-default-sorting-mode 'major-mode)
     (setq ibuffer-always-show-last-buffer t)
     (setq ibuffer-view-ibuffer t)
     (setq ibuffer-saved-filter-groups
           (quote (("default"      
                    ("Org" ;; all org-related buffers
                     (mode . org-mode))  
                    ("Programming" ;; prog stuff not already in MyProjectX
                     (or
                      (mode . c-mode)
                      (mode . html-mode)
                      (mode . c++-mode)
                      (mode . perl-mode)
                      (mode . python-mode)
                      (mode . emacs-lisp-mode)
                      ;; etc
                      )) 
                    ("ERC"   (mode . erc-mode))
                    ("Directories"   (mode . dired-mode))
                    ("Shell"
                     (or
                      (mode . shell-mode)
                      (mode . term-mode)))))))

     (add-hook 'ibuffer-mode-hook
               (lambda ()
                 (ibuffer-switch-to-saved-filter-groups "default")))
     ))

(global-set-key (kbd "C-x C-b") 'ibuffer-other-window)