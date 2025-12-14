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

(defun comint-send-file-base64 (src-path dst-path perms)
  (let* ((src-data (base64-encode-string
                    (with-temp-buffer
                      (insert-file-contents src-path)
                      (buffer-string))
                    t))
         (current-proc (get-buffer-process (current-buffer)))
         (cmd (format "echo %s | base64 -d > %s;%s"
                      src-data
                      dst-path
                      (if perms
                          (format " chmod %s %s; " perms dst-path)
                        " "))))
    (insert cmd)
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

(autoload 'defhydra "hydra" nil t)

(defun setenv-from-bashrc (name)
  (setenv
   name
   (shell-command-to-string
    (format "bash -c '. ~/.bashrc; echo -n $%s'" name))))

(defmacro with-ido-completion (fun)
  "Wrap FUN in another interactive function with ido completion."
  `(defun ,(intern (concat (symbol-name fun) "/with-ido")) ()
     ,(format "Forward to `%S' with ido completion." fun)
     (interactive)
     (let ((completing-read-function
            'ido-occasional-completing-read))
       (call-interactively #',fun))))
