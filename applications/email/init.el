
;; something about ourselves
(setq
 email-send-addresses '("user@gmail.com")
 auth-sources '("~/.authinfo.gpg")
 user-full-name  "xxx yyy"
 message-signature
 (concat
  "Best Regards,\n"
  "xxx yyy\n"))

(defvar *user-mails*
  "user@gmail\\.com\\|user2@gmail\\.com")


(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

(defun email-send-switch (&optional email)
  (interactive)
  (let ((user-mail (or email
                       (ido-completing-read
                        "switch email to: " email-send-addresses))))
    (setq user-mail-address user-mail
          smtpmail-smtp-user user-mail
          smtpmail-auth-credentials `(("smtp.gmail.com" 587 ,user-mail nil)))))

;; by default first element in mail-addresses is used
(email-send-switch (car email-send-addresses))

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

(setq auth-source-cache-expiry nil
      auth-source-save-behavior nil)


(when (file-exists-p "~/tools/mu/mu4e")
  (progn
    (setq load-path (append (list "~/tools/mu/mu4e") load-path))
    (autoload 'mu4e "mu4e" nil t)
    (eval-after-load "mu4e"
      '(progn
         (load "mu4e-init")))))

(eval-after-load "gnus"
  '(progn
     (load "gnus-init")))


(defun dot-offlineimap ()
  (interactive)
  (comint-send-file
   (in-emacs-d "applications/email/.offlineimaprc")
     "~/.offlineimaprc")
  (comint-send-file
   (in-emacs-d "applications/email/.offlineimaprc.py")
     "~/.offlineimaprc.py")
  )
