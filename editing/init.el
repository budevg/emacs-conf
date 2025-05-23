(setq initial-scratch-message
      (format ";;\n;; The time is %s\n;; Welcome root !!!\n;;\n"
              (format-time-string "%Y-%m-%d, %T")))
(setq frame-title-format
  '((:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name))
                  "%b"))))

(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

(set-default 'fill-column 80)
(defun unfill-toggle ()
  "Toggle filling/unfilling of the current region.
Operates on the current paragraph if no region is active."
  (interactive)
  (let (deactivate-mark
        (fill-column
         (if (eq last-command this-command)
             (progn (setq this-command nil)
                    most-positive-fixnum)
           fill-column)))
    (call-interactively 'fill-paragraph)))
(define-key esc-map "q" 'unfill-toggle)

(global-hi-lock-mode 1)


(global-set-key [(control meta b)] 'toggle-truncate-lines)
(global-set-key [(control z)] 'undo)
(global-set-key [(meta g)] 'goto-line)

(global-set-key [(meta s)] 'occur)
;; Activate occur easily inside isearch
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(autoload 'iedit-mode "iedit" nil t)
(global-set-key [(control %)] 'iedit-mode)

(global-set-key [(meta /)] 'hippie-expand)
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)
(delete 'try-complete-lisp-symbol-partially hippie-expand-try-functions-list)
(delete 'try-complete-lisp-symbol hippie-expand-try-functions-list)


(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

(global-set-key "\C-w" 'backward-delete-word)

(defun copy-loc ()
  (interactive)
  (let* ((f (buffer-file-name))
         (l (line-number-at-pos))
         (s (format "%s:%s" f l))
         )
    (kill-new s)
    (message s)))

(global-set-key (kbd "M-e") 'copy-loc)

(defun switch-to-hexl-mode ()
  (interactive)
  (hexl-mode))

(global-set-key [(control meta x)] 'switch-to-hexl-mode)


(autoload 'browse-kill-ring "browse-kill-ring" nil t)
(global-set-key [(meta delete)] 'browse-kill-ring)
(eval-after-load "browse-kill-ring"
  '(progn
     (define-key browse-kill-ring-mode-map [down] 'browse-kill-ring-forward)
     (define-key browse-kill-ring-mode-map [(control down)] 'browse-kill-ring-forward)
     (define-key browse-kill-ring-mode-map [up] 'browse-kill-ring-previous)
     (define-key browse-kill-ring-mode-map [(control up)] 'browse-kill-ring-previous)
     (setq browse-kill-ring-display-duplicates nil)))


; autosave settings
(setq auto-save-list-file-prefix nil)
(setq make-backup-files nil)

; recentf - save history of recently visited files
;;(autoload 'recentf-mode "recentf.el" nil t)
;;(autoload 'recentf-save-list "recentf.el" nil t)
;;(run-with-idle-timer (* 5 60) t 'recentf-save-list)
;;(setq recentf-auto-cleanup 'never)
;;(setq recentf-max-saved-items 1000)

;; disable autoencoding on <?xml version="1.0" encoding="UTF-16" ?> line
(setq auto-coding-alist (append auto-coding-alist '(("\\.xml\\'" . no-conversion))))

;; ignore logging warnings when copy too much to the buffer
(setq warning-suppress-log-types '((undo discard-info)))

(setq ispell-program-name "aspell")

(run-with-idle-timer
 2 nil
 (lambda ()
   (defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))
   (defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active
        (list (region-beginning) (region-end))
      (list (line-beginning-position) (line-beginning-position 2)))))

   (require 'uniquify)
   (setq uniquify-buffer-name-style 'post-forward)
   ))

(global-set-key [(meta i)] 'align)

(defun insert-date()
  "Insert a time-stamp according to locale's date and time format."
  (interactive)
  (insert (format-time-string "%d/%m/%Y, %k:%M" (current-time))))

(global-set-key "\C-cd" 'insert-date)

(setq x-select-enable-clipboard t)

;; move line up/or down
(autoload 'drag-stuff-mode "drag-stuff" nil t)
(global-set-key [(meta w)]
                (lambda () (interactive)
                  (if mark-active
                      (call-interactively 'indent-rigidly)
                    (call-interactively 'drag-stuff-mode))))

;; join lines
(global-set-key (kbd "M-j")
            (lambda ()
              (interactive)
              (join-line -1)))

;; don't use lame windows key bindings
(setq cua-enable-cua-keys nil)
;; Edit text rectangles in emacs
(cua-mode)

;; no trailing spaces at the end
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; latitude E6530 shortcuts
;;(global-set-key (kbd "<C-prior>") 'kill-ring-save)
;;(global-set-key (kbd "<S-prior>") 'cua-paste)
;;(global-set-key (kbd "<S-next>") 'kill-region)

(autoload 'math-symbols "unicode-abbrevs" nil t)

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(global-set-key (kbd "C-x n") 'narrow-or-widen-dwim)
