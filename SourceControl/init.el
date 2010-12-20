
;; empty backends for vc
;; since that introduces overhead in opening file
(setq vc-handled-backends '())

(autoload 'svn-status "psvn" "svn-status autoload" t)
(autoload 'git-status "git" "git-status autoload" t)
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)
(autoload 'ahg-status "ahg" "ahg-status autoload" t)

