
;; empty backends for vc
;; since that introduces overhead in opening file
(setq vc-handled-backends '())

(defun dot-gitconfig ()
  (interactive)
  (comint-send-file
   (in-emacs-d "source_control/.gitconfig")
   "~/.gitconfig"))

(autoload 'browse-at-remote "browse-at-remote" nil t)
(global-set-key (kbd "C-f a") 'browse-at-remote)

;; diff

(defun diff-current-buffer-with-file ()
  (interactive)
  (let ((diff-switches "-u"))
    (diff-buffer-with-file (current-buffer))))

(global-set-key [(control meta =)] 'diff-current-buffer-with-file)

(eval-after-load "diff-mode"
  '(progn
     (add-hook 'diff-mode-hook
               (lambda ()
                 (setq diff-auto-refine-mode nil)))))
