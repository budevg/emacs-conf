
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-log-done t)

(defun org-config-beamer ()
  (require 'org-export-latex)
  (add-to-list 'org-export-latex-classes
               '("beamer" 
     "\\documentclass[11pt]{beamer}\n
      \\mode<{{{beamermode}}}>\n
      \\usetheme{{{{beamertheme}}}}\n
      \\usecolortheme{{{{beamercolortheme}}}}\n
      \\beamertemplateballitem\n
      \\setbeameroption{show notes}
      \\usepackage[utf8]{inputenc}\n
      \\usepackage[T1]{fontenc}\n
      \\usepackage{hyperref}\n
      \\usepackage{color}
      \\usepackage{listings}
      \\lstset{numbers=none,language=[ISO]C++,tabsize=4,
  frame=single,
  basicstyle=\\small,
  showspaces=false,showstringspaces=false,
  showtabs=false,
  keywordstyle=\\color{blue}\\bfseries,
  commentstyle=\\color{red},
  }\n
      \\usepackage{verbatim}\n
      \\institute{{{{beamerinstitute}}}}\n          
       \\subject{{{{beamersubject}}}}\n" 
     ("\\section{%s}" . "\\section*{%s}")
     
     ("\\begin{frame}[fragile]\\frametitle{%s}" 
       "\\end{frame}" 
       "\\begin{frame}[fragile]\\frametitle{%s}" 
       "\\end{frame}"))))

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

     (org-defkey org-mode-map [(shift home)] 'move-beginning-of-line)
     (org-defkey org-mode-map [(shift end)] 'move-end-of-line)


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

     (setq org-cycle-emulate-tab 'exc-hl-bol)

     (setq org-blank-before-new-entry '((heading . nil)
                                        (plain-list-item . auto)))
     
     (setq org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d)")
                                     (sequence "NEXT(n)" "WAITING(w)" "SOMEDAY(S)"
                                               "ARCHIVE(a)" "|" "CANCELED(c)"))))

     (setq org-todo-keyword-faces (quote (("TODO" :foreground "red" :weight bold)
                                          ("STARTED" :foreground "orange" :weight bold)
                                          ("DONE" :foreground "green" :weight bold)
                                          ("WAITING" :foreground "orange" :weight bold)
                                          ("SOMEDAY" :foreground "magenta" :weight bold)
                                          ("NEXT" :foreground "forest green" :weight bold)
                                          ("ARCHIVE" :foreground "red" :weight bold)
                                          ("CANCELED" :foreground "magenta" :weight bold))))
     (setq org-descriptive-links nil)
     (setq org-ditaa-jar-path (expand-file-name
                               "~/.emacs.d/Publishing/org/scripts/ditaa.jar"))
     (setq org-export-htmlize-output-type 'css)
     (org-config-beamer)
     ))

(setq org-directory "~/.org/")
(setq org-default-notes-file "~/.org/.notes")
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(add-hook 'remember-mode-hook 'org-remember-apply-template)
(setq org-remember-templates
      '(("Todo" ?t "* TODO %?" "~/.org/TODO.org" "Tasks")
        ("Journal" ?j "* %U %?" "~/.org/JOURNAL.org" "Events")
        ("Idea" ?i "* %? %U\n" "~/.org/JOURNAL.org" "Ideas")
        ("Pomodoro" ?p "* %U\n** %?" "~/.org/POMODORO.org" "Tasks")))
(define-key global-map "\C-cr" 'org-remember)

