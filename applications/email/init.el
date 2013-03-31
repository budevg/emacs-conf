(when (file-exists-p "~/tools/mu/mu4e")
  (progn
    (setq load-path (append (list "~/tools/mu/mu4e") load-path))
    (autoload 'mu4e "mu4e" nil t)

    (setq mu4e-maildir (expand-file-name "~/Maildir"))

    (setq mu4e-drafts-folder "/[Gmail].Drafts")
    (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
    (setq mu4e-trash-folder  "/[Gmail].Trash")

    ;; don't save message to Sent Messages, Gmail/IMAP will take care of this
    (setq mu4e-sent-messages-behavior 'delete)

    ;; setup some handy shortcuts
    (setq mu4e-maildir-shortcuts
          '( ("/INBOX"               . ?i)
             ("/[Gmail].Sent Mail"   . ?s)
             ("/[Gmail].Trash"       . ?t)
             ("/[Gmail].All Mail"    . ?a)))

    ;; allow for updating mail using 'U' in the main view:
    (setq mu4e-get-mail-command "offlineimap")

    ;; something about ourselves
    (setq
     user-mail-address "user@gmail.com"
     user-full-name  "xxx yyy"
     message-signature
     (concat
      "Best Regards,\n"
      "xxx yyy\n"))

    (setq message-send-mail-function 'smtpmail-send-it
          smtpmail-stream-type 'starttls
          smtpmail-smtp-user "xxx@gmail.com"
          smtpmail-auth-credentials '(("smtp.gmail.com" 587 "xxx@gmail.com" nil))
          smtpmail-default-smtp-server "smtp.gmail.com"
          smtpmail-smtp-server "smtp.gmail.com"
          smtpmail-smtp-service 587)

    ;; don't keep message buffers around
    (setq message-kill-buffer-on-exit t)

    (setq auth-source-cache-expiry nil
          auth-source-save-behavior nil)
    ))

(defun dot-offlineimap ()
  (interactive)
  (comint-send-file
   "~/.emacs.d/Email/.offlineimaprc"
     "~/.offlineimaprc")
  (comint-send-file
   "~/.emacs.d/Email/.offlineimaprc.py"
     "~/.offlineimaprc.py")
  )
