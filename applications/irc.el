(autoload 'erc "erc" "erc autoload" t)

(eval-after-load "erc"
  '(progn
     (setq erc-modules '(autojoin button completion fill irccontrols match menu netsplit noncommands ring stamp track))
     (setq erc-filtered-content '("\\*\\*\\*.*has joined channel.*"
                                  "\\*\\*\\*.*has quit:.*"
                                  "\\*\\*\\*.*is now known as.*"
                                  "\\*\\*\\*.*has left channel.*"
                                  ))
     (defun erc-filter-content (msg)
       "Check whether MSG is foolish."
       (erc-list-match erc-filtered-content msg))

     (add-hook 'erc-insert-pre-hook
               (lambda (s)
                 (when (erc-filter-content s)
                   (setq erc-insert-this nil))))

     (setq erc-server-history-list '("irc.freenode.net" "irc.oftc.net" "irc.run.net"))
     (setq erc-autojoin-channels-alist '())
     (setq erc-hide-prompt t)
     (define-key erc-mode-map [(control up)] 'erc-previous-command)
     (define-key erc-mode-map [(control down)] 'erc-next-command)
     (define-key erc-mode-map [home] nil)))
