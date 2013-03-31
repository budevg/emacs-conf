
;; empty backends for vc
;; since that introduces overhead in opening file
(setq vc-handled-backends '())

(autoload 'svn-status "psvn" "svn-status autoload" t)
(autoload 'git-status "git" "git-status autoload" t)
(defun git-status-on-current-dir ()
  (interactive)
  (git-status default-directory))
(global-set-key [(meta M)] 'git-status-on-current-dir)
(autoload 'ahg-status "ahg" "ahg-status autoload" t)

(eval-after-load "psvn"
  '(progn
     (define-key svn-status-mode-map (kbd "TAB") 'svn-status-show-svn-diff)))

(defun dot-gitconfig ()
  (interactive)
  (comint-send-file
   "~/.emacs.d/SourceControl/.gitconfig"
   "~/.gitconfig"))
