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

(defun message-notify (title msg &optional expiration-time)
  "Show a popup if we're on X, or echo it otherwise; TITLE is the title
 of the message, MSG is the context."

  (interactive)
  (cond ((and (eq window-system 'x) (executable-find "notify-send"))
         (let ((time (if expiration-time expiration-time 1800000)))
           (shell-command
            (format "notify-send -t %d '%s' '%s'" time title msg))))
        (t (message (concat title ": " msg)))))
