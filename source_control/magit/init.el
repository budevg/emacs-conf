(autoload 'magit-status "magit" nil t)
(global-set-key [(meta m)] 'magit-status)

(setq magit-version "4.3.0")

(eval-after-load "magit"
  '(progn
     (setq-default diff-auto-refine-mode nil)
     (setq magit-push-always-verify nil)
     (setq magit-section-visibility-indicator nil)
     (setq magit-popup-show-common-commands nil)
     (setq magit-diff-highlight-hunk-body nil)
     (setq magit-save-repository-buffers nil)
     (setq magit-revert-buffers 'silent)
     (setq magit-revision-show-gravatars nil)
     (setq magit-delete-by-moving-to-trash nil)
     (custom-set-faces
      '(magit-section-highlight ((t (:inherit nil)))))
     (setq magit-status-sections-hook
           '(magit-insert-status-headers
             magit-insert-stashes
             magit-insert-merge-log
             magit-insert-rebase-sequence
             magit-insert-am-sequence
             magit-insert-sequencer-sequence
             magit-insert-bisect-output
             magit-insert-bisect-rest
             magit-insert-bisect-log
             magit-insert-untracked-files
             magit-insert-unstaged-changes
             magit-insert-staged-changes
             magit-insert-unpulled-from-upstream
             magit-insert-unpulled-from-pushremote
             magit-insert-unpushed-to-upstream
             magit-insert-unpushed-to-pushremote))

     (setq magit-status-headers-hook
           '(magit-insert-error-header
             magit-insert-diff-filter-header
             magit-insert-head-branch-header
             magit-insert-upstream-branch-header
             magit-insert-push-branch-header
             magit-insert-tags-header
             magit-insert-remote-header))

     (setq git-commit-setup-hook
           '(git-commit-save-message
             git-commit-setup-changelog-support
             git-commit-turn-on-auto-fill
             git-commit-propertize-diff
             with-editor-usage-message
             git-commit-turn-on-flyspell
             ))

     (setq magit-diff-auto-show '())

     (setq git-commit-finish-query-functions '())

     (define-key magit-mode-map [C-tab] nil)
     (define-key magit-mode-map "q"
       (lambda ()
         (interactive)
         (magit-mode-bury-buffer t)))
     (global-set-key (kbd "C-f m") 'magit-file-dispatch)

     (defadvice magit-status (around magit-fullscreen activate)
       ad-do-it
       (delete-other-windows))
     (autoload 'magit-gitignore-popup "magit-gitignore" nil t)

     (defun magit-project-find-function (dir)
       (let ((root (magit-toplevel dir)))
         (and root (cons 'transient root))))
     (with-eval-after-load 'project
       (add-to-list 'project-find-functions 'magit-project-find-function t))
     ))
