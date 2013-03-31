(require 'cl)

(defun autoload-and-run (symbol file interactive callback)
  (autoload symbol file nil interactive)
  (eval-after-load (symbol-name symbol) callback))


(defun comint-send-file (src-path dst-path)
  (let ((src-data (with-temp-buffer
                   (insert-file-contents src-path)
                   (buffer-string)))
        (current-proc (get-buffer-process (current-buffer))))
    (end-of-buffer)
    (comint-send-string current-proc (format "cat > %s\n" dst-path))
    (comint-send-string current-proc src-data)
    (comint-send-input nil t)
    (comint-send-input nil t)
    (comint-send-eof)
    ))

(defun message-notify (title msg)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
 of the message, MSG is the context."

  (interactive)
   (cond ((eq window-system 'x)
          (cond ((executable-find "notify-send") (shell-command
                                                  (format "notify-send -t 1800000 '%s' '%s'" title msg)))
                ((message-box (concat title ": " msg)))))
         (t (message (concat title ": " msg)))))
