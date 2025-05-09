(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-capture-templates
   '(("t" "TODO" entry (file "~/.org/todo.org")
      "* %U %?")
     ))
  (org-directory "~/.org/")
  (org-display-custom-times t)
  (org-time-stamp-custom-formats '("<%d/%m/%Y>" . "<%d/%m/%Y %H:%M>"))
  :config
  (setq org-adapt-indentation t
        org-confirm-babel-evaluate nil
        org-cycle-emulate-tab 'exc-hl-bol

        org-blank-before-new-entry '((heading . nil)
                                     (plain-list-item . auto))

        org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                                  (sequence "NEXT(n)" "WAITING(w)" "SOMEDAY(S)"
                                            "ARCHIVE(a)" "|" "CANCELED(c)")))

        org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                       ("STARTED" :foreground "orange" :weight bold)
                                       ("DONE" :foreground "green" :weight bold)
                                       ("WAITING" :foreground "orange" :weight bold)
                                       ("SOMEDAY" :foreground "magenta" :weight bold)
                                       ("NEXT" :foreground "forest green" :weight bold)
                                       ("ARCHIVE" :foreground "red" :weight bold)
                                       ("CANCELED" :foreground "magenta" :weight bold)))
        org-descriptive-links nil
        org-export-htmlize-output-type 'css
        org-export-creator-info nil)

  ;;(org-babel-do-load-languages
  ;; 'org-babel-load-languages
  ;; '((emacs-lisp . t)
  ;;   (shell . t)
  ;;   (python . t)))

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

  ;;(org-defkey org-mode-map [(kp-6)] 'org-shiftcontrolright)
  ;;(org-defkey org-mode-map [(kp-4)]  'org-shiftcontrolleft)

  ;;(org-defkey org-mode-map [(shift home)] 'move-beginning-of-line)
  ;;(org-defkey org-mode-map [(shift end)] 'move-end-of-line)


  ;; undef old org keys
  (org-defkey org-mode-map [(meta left)]  nil)
  (org-defkey org-mode-map [(meta right)] nil)
  (org-defkey org-mode-map [(meta up)]    nil)
  (org-defkey org-mode-map [(meta down)]  nil)
  (org-defkey org-mode-map (kbd "C-x n") nil)

  (org-defkey org-mode-map [(meta shift left)]   nil)
  (org-defkey org-mode-map [(meta shift right)]  nil)
  (org-defkey org-mode-map [(meta shift up)]     nil)
  (org-defkey org-mode-map [(meta shift down)]   nil)

  (org-defkey org-mode-map [(shift up)]          nil)
  (org-defkey org-mode-map [(shift down)]        nil)
  (org-defkey org-mode-map [(shift left)]        nil)
  (org-defkey org-mode-map [(shift right)]       nil)

  (org-defkey org-mode-map [(control shift right)] nil)
  (org-defkey org-mode-map [(control shift left)]  nil))
