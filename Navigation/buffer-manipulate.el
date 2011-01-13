
(autoload 'windmove-find-other-window "windmove" nil t)
(defun windmove-ext (dir)
  (interactive)
  (let ((window (windmove-find-other-window dir))
        (offset (if (eq dir 'up)
                    -1
                  1)))
    (cond ((null window)
           (other-window offset))
          ((and (window-minibuffer-p window)
                (not (minibuffer-window-active-p window)))
           (other-window offset))
          (t
           (select-window window)))))

(global-set-key [(control tab)] 'other-window)
(global-set-key [(meta up)] '(lambda () (interactive) (windmove-ext 'up)))
(global-set-key [(meta down)] '(lambda () (interactive) (windmove-ext 'down)))
(global-set-key [(pause)] 'kill-this-buffer)
(global-set-key [(control pause)] 'delete-frame)

(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have
(icomplete-mode t)

(global-set-key [(control \;)] 'ido-switch-buffer)
(global-set-key [(control \])] 'ido-find-file)


(global-set-key [(control right)] 'forward-sexp)
(global-set-key [(control left)]  'backward-sexp)

(global-set-key [(meta pause)] 'delete-window)
(global-set-key [f5]  'delete-other-windows)
(global-set-key [(control f5)]  'new-frame)

(fset 'yes-or-no-p 'y-or-n-p)

(autoload 'buf-move-up "buffer-move" nil t)
(autoload 'buf-move-down "buffer-move" nil t)
(autoload 'buf-move-lef "buffer-move" nil t)
(autoload 'buf-move-right "buffer-move" nil t)
(global-set-key [(control kp-8)] 'buf-move-up)
(global-set-key [(control kp-2)] 'buf-move-down)
(global-set-key [(control kp-4)] 'buf-move-left)
(global-set-key [(control kp-6)] 'buf-move-right)


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
                    ("Text" 
                     (mode . text-mode))
                    ("Programming" ;; prog stuff not already in MyProjectX
                     (or
                      (mode . c-mode)
                      (mode . html-mode)
                      (mode . c++-mode)
                      (mode . perl-mode)
                      (mode . python-mode)
                      (mode . emacs-lisp-mode)
                      (mode . latex-mode)
                      ;; etc
                      ))
                    ("SourceControl"
                     (or
                      (mode . cvs-mode)
                      (mode . git-blame-mode)
                      (mode . git-status-mode)
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

;; rotate buffers
(global-set-key [(meta right)] 'previous-buffer)
(global-set-key [(meta left)]  'next-buffer)
