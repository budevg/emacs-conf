
(column-number-mode t)
(line-number-mode t)
(size-indication-mode t)

(set-default 'fill-column 80)

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


(global-set-key [(meta /)] 'hippie-expand)
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)


(defun delete-word (arg)
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  (interactive "p")
  (delete-word (- arg)))

(global-set-key "\C-w" 'backward-delete-word)

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


(autoload 'kill-ring-search "kill-ring-search" nil t)
(global-set-key [(meta insert)] 'kill-ring-search)

(setq make-backup-files nil)

;; disable autoencoding on <?xml version="1.0" encoding="UTF-16" ?> line
(setq auto-coding-alist (append auto-coding-alist '(("\\.xml\\'" . no-conversion))))

;; ignore logging warnings when copy too much to the buffer
(setq warning-suppress-log-types '((undo discard-info)))

(defun insert-timestamp ()
  (interactive)
  (insert (format-time-string "%d/%m/%Y %R")))
	
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

(setq ispell-program-name "aspell")

(defadvice kill-ring-save (before slick-copy activate compile) "When called
  interactively with no active region, copy a single line instead."
  (interactive (if mark-active (list (region-beginning) (region-end)) (message
  "Copied line") (list (line-beginning-position) (line-beginning-position
  2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))
