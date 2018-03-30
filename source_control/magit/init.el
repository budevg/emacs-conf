(autoload 'magit-status "magit" nil t)
(global-set-key [(meta m)] 'magit-status)

(defun magit-section-toggle-new ()
  (interactive)
  (let ((file (ffap-file-at-point)))
    (if file
        (magit-diff "HEAD" '() (list file))
      (magit-section-toggle (magit-current-section)))))

(setq magit-version "2.12")

(eval-after-load "magit"
  '(progn
     (setq-default diff-auto-refine-mode nil)
     (setq magit-push-always-verify nil)
     (setq magit-popup-show-common-commands nil)
     (setq magit-diff-highlight-hunk-body nil)
     (setq magit-save-repository-buffers nil)
     (setq magit-revert-buffers 'silent)
     (setq magit-revision-show-gravatars nil)
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

     (define-key magit-mode-map "\t" 'magit-section-toggle-new)
     (define-key magit-mode-map [C-tab] nil)
     (define-key magit-mode-map "q"
       (lambda ()
         (interactive)
         (magit-mode-bury-buffer t)))
     (defadvice magit-status (around magit-fullscreen activate)
       ad-do-it
       (delete-other-windows))
     ))
