
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(eval-after-load 'org
  '(progn
     (org-defkey org-mode-map "\C-m"     'org-return-indent)
     (org-defkey org-mode-map [(control tab)] 'other-window)))