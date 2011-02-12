;;; Autoloads for magit

(autoload 'magit-status "magit" nil t)
(setq magit-save-some-buffers nil)
(global-set-key [(meta m)] 'magit-status)

(defun magit-toggle-section-new ()
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (git-setup-diff-buffer
         (apply #'git-run-command-buffer "*git-diff*" "diff-index" "-p" "-M" "HEAD" "--" (list file)))
      (magit-toggle-section))))

(eval-after-load "magit"
  '(progn
     (autoload 'git-setup-diff-buffer "git" nil t)
     (autoload 'git-run-command-buffer "git" nil t)
     (define-key magit-mode-map (kbd "TAB") 'magit-toggle-section-new)
     (define-key magit-mode-map (kbd "<M-left>") nil)
     ))
