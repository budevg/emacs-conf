
(setq message-send-mail-function 'smtpmail-send-it
      message-kill-buffer-on-exit t
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
           (defun mu4e-account-context (prefix)
             (interactive)
             (make-mu4e-context
              :name prefix
              :enter-func `(lambda () (mu4e-account ,prefix))
              ))
           (defun mu4e-account (prefix)
             (interactive)
             (mu4e-message (format "mu4e-account %s" prefix))
             (cond
              ((string= prefix "AAA")
               (setq
                mu4e-inbox-folder (format "/%s/Inbox" prefix)
                mu4e-drafts-folder (format "/%s/[Gmail]/Drafts" prefix)
                mu4e-sent-folder (format "/%s/[Gmail]/Sent Mail" prefix)
                mu4e-trash-folder (format "/%s/[Gmail]/Trash" prefix)
                smtpmail-smtp-server "smtp.gmail.com"
                user-mail-address (format "%s@gmail.com" prefix)
                user-full-name  prefix)
               )
              ((string= prefix "BBB")
               (setq
                mu4e-inbox-folder (format "/%s/Inbox" prefix)
                mu4e-drafts-folder (format "/%s/Drafts" prefix)
                mu4e-sent-folder (format "/%s/Sent" prefix)
                mu4e-trash-folder (format "/%s/Deleted" prefix)
                smtpmail-smtp-server "smtp.office365.com"
                user-mail-address (format "%s@outlook.com" prefix)
                user-full-name prefix
                )))

             (setq
              mu4e-maildir-shortcuts `((,mu4e-inbox-folder . ?i)
                                       (,mu4e-sent-folder  . ?s)
                                       (,mu4e-trash-folder . ?t)
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
                                  :name "Flagged messages"
                                  :query "flag:flagged"
                                  :key ?f))

              mu4e-compose-signature (format "Best Regards,\n%s\n" user-full-name)


              mu4e-get-mail-command (format "mbsync -c %s %s" (in-emacs-d "applications/email/.mbsyncrc") prefix)

              smtpmail-default-smtp-server smtpmail-smtp-server
              smtpmail-local-domain smtpmail-smtp-server
              smtpmail-smtp-user user-mail-address
              smtpmail-stream-type 'starttls
              smtpmail-smtp-service 587
              )
             )
           (setq
                 mu4e-maildir (expand-file-name "~/scratch/Mail")
                 mu4e-change-filenames-when-moving t
                 mu4e-view-show-addresses t
                 ;;mu4e-sent-messages-behavior 'delete

                 mu4e~get-mail-password-regexp "^Password.*: $"
                 mu4e-contexts
                 `( ,(mu4e-account-context "AAA")
                    ,(mu4e-account-context "BBB")
                    )

                 mu4e-context-policy 'pick-first
                 mu4e-view-actions
                 '(("view in browser" . mu4e-action-view-in-browser)
                   ("show this thread" . mu4e-action-show-thread))
                 )
           (define-key mu4e-view-mode-map (kbd "C-w")
             (lambda ()
               (interactive)
               (let ((url (get-text-property (point) 'shr-url)))
                 (when url
                   (kill-new url)
                   (message "%s" url))
                 ))
             )
           )))))
