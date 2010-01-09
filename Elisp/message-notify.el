(defun message-notify (title msg)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
 of the message, MSG is the context."
 
  (interactive)
   (cond ((eq window-system 'x)
          (cond ((executable-find "notify-send") (shell-command
                                                  (format "notify-send '%s' '%s'" title msg)))
                ((message-box (concat title ": " msg)))))
         (t (message (concat title ": " msg)))))