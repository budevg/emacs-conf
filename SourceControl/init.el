
;; empty backends for vc
;; since that introduces overhead in opening file
(setq vc-handled-backends '())

(autoload 'svn-status "psvn" "svn-status autoload" t)
(autoload 'git-status "git" "git-status autoload" t)
(autoload 'magit-status "magit" "magit-status autoload" t)

