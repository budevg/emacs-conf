
(setq user-mail-address "xxx@yyy.com"
      user-full-name  "xxx yyy"
      message-signature (concat
                         "Best Regards,\n"
                         "xxx yyy\n")
      )

(setq message-send-mail-function 'smtpmail-send-it
      message-kill-buffer-on-exit t
      smtpmail-default-smtp-server "xxx.com"
      smtpmail-local-domain "xxx.com"
      smtpmail-smtp-user "xxx"
      smtpmail-smtp-server "xxx.com"
      smtpmail-stream-type 'starttls
      smtpmail-smtp-service 1587
      )

(setq auth-source-cache-expiry nil
      auth-source-save-behavior nil
      auth-sources '("~/.authinfo.gpg")
      )



(when (executable-find "mu")
  (let* ((mu-dir (file-truename (file-name-directory (executable-find "mu"))))
         (mu4e-lisp-dir (file-truename (concat mu-dir "../share/emacs/site-lisp/mu4e")))
         )
    (progn
      (add-to-list 'load-path mu4e-lisp-dir)
      (autoload 'mu4e "mu4e" nil t)
      (eval-after-load "mu4e"
        '(progn
           (setq mu4e-maildir (expand-file-name "~/Mail")

                 mu4e-inbox-folder  "/inbox"
                 mu4e-drafts-folder "/drafts"
                 mu4e-sent-folder   "/sent"
                 mu4e-trash-folder  "/deleted"

                 ;;mu4e-sent-messages-behavior 'delete

                 mu4e-maildir-shortcuts '(("/inbox" . ?i)
                                          ("/sent"  . ?s)
                                          ("/deleted" . ?t)
                                          )

                 mu4e-bookmarks `( ,(make-mu4e-bookmark
                                     :name  "Unread messages"
                                     :query "flag:unread AND NOT flag:trashed"
                                     :key ?u)
                                   ,(make-mu4e-bookmark
                                     :name "Today's messages"
                                     :query "date:today..now"
                                     :key ?t)
                                   ,(make-mu4e-bookmark
                                     :name "Last 7 days"
                                     :query "date:7d..now"
                                     :key ?w)
                                   ,(make-mu4e-bookmark
                                     :name "Flagged messages"
                                     :query "flag:flagged"
                                     :key ?f))


                 mu4e~get-mail-password-regexp "^Password: $"
                 mu4e-get-mail-command "offlineimap"
                 )
           )))))

(defun dot-offlineimap ()
  (interactive)
  (comint-send-file
   (in-emacs-d "applications/email/.offlineimaprc")
     "~/.offlineimaprc")
  (comint-send-file
   (in-emacs-d "applications/email/.offlineimaprc.py")
     "~/.offlineimaprc.py")
  )
