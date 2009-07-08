
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)
(eval-after-load 'org
  '(progn
     (org-defkey org-mode-map "\C-m"     'org-return-indent)
     (org-defkey org-mode-map [(control tab)] 'other-window)
     
     (org-defkey org-mode-map [(meta kp-4)]  'org-metaleft)
     (org-defkey org-mode-map [(meta kp-6)] 'org-metaright)
     (org-defkey org-mode-map [(meta kp-8)]    'org-metaup)
     (org-defkey org-mode-map [(meta kp-2)]  'org-metadown)
     
     (org-defkey org-mode-map [(meta control kp-4)]   'org-shiftmetaleft)
     (org-defkey org-mode-map [(meta control kp-6)]  'org-shiftmetaright)
     (org-defkey org-mode-map [(meta control kp-8)]     'org-shiftmetaup)
     (org-defkey org-mode-map [(meta control kp-2)]   'org-shiftmetadown)

     (org-defkey org-mode-map [(control kp-8)]          'org-shiftup)
     (org-defkey org-mode-map [(control kp-2)]        'org-shiftdown)
     (org-defkey org-mode-map [(control kp-4)]        'org-shiftleft)
     (org-defkey org-mode-map [(control kp-6)]       'org-shiftright)

     (org-defkey org-mode-map [(kp-6)] 'org-shiftcontrolright)
     (org-defkey org-mode-map [(kp-4)]  'org-shiftcontrolleft)

     ;; undef old org keys
     (org-defkey org-mode-map [(meta left)]  nil)
     (org-defkey org-mode-map [(meta right)] nil)
     (org-defkey org-mode-map [(meta up)]    nil)
     (org-defkey org-mode-map [(meta down)]  nil)

     (org-defkey org-mode-map [(meta shift left)]   nil)
     (org-defkey org-mode-map [(meta shift right)]  nil)
     (org-defkey org-mode-map [(meta shift up)]     nil)
     (org-defkey org-mode-map [(meta shift down)]   nil)
 
     (org-defkey org-mode-map [(shift up)]          nil)
     (org-defkey org-mode-map [(shift down)]        nil)
     (org-defkey org-mode-map [(shift left)]        nil)
     (org-defkey org-mode-map [(shift right)]       nil)

     (org-defkey org-mode-map [(control shift right)] nil)
     (org-defkey org-mode-map [(control shift left)]  nil)

     (setq org-blank-before-new-entry '((heading . nil)
                                        (plain-list-item . auto)))

     ))