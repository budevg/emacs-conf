(autoload 'magit-status "magit" nil t)
(autoload 'rebase-mode "rebase-mode" nil t)
(autoload 'magit-blame-mode "magit-blame" nil t)
(add-to-list 'auto-mode-alist
             '("git-rebase-todo" . rebase-mode))

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
     (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
     ))

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit
buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))
