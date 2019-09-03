
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
           (setq
                 mu4e-maildir (expand-file-name "~/scratch/Mail")

                 ;;mu4e-sent-messages-behavior 'delete

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


                 mu4e~get-mail-password-regexp "^Password.*: $"
                 mu4e-contexts
                 `( ,(make-mu4e-context
                      :name "AAA"
                      :enter-func (lambda () (mu4e-message "Enter AAA"))
                      :leave-func (lambda () (mu4e-message "Leave AAA"))
                      ;; we match based on the contact-fields of the message
                      :match-func (lambda (msg)
                                    (when msg
                                      (mu4e-message-contact-field-matches
                                       msg
                                       :to "AAA@xxx.com")))
                      :vars '( (user-mail-address . "AAA@xxx.com")
                               (user-full-name	    . "AAA yyy")
                               (mu4e-compose-signature .
                                                        (concat
                                                         "Best Regards,\n"
                                                         "AAA yyy\n"))
                               (mu4e-inbox-folder ."/AAA/Inbox")
                               (mu4e-drafts-folder . "/AAA/Drafts")
                               (mu4e-sent-folder .   "/AAA/Sent")
                               (mu4e-trash-folder .  "/AAA/Deleted")

                               (mu4e-maildir-shortcuts . (("/AAA/Inbox" . ?i)
                                                          ("/AAA/Sent"  . ?s)
                                                          ("/AAA/Deleted" . ?t)
                                                          ))


                               (mu4e-get-mail-command . "mbsync -c ~/.emacs.d/applications/email/.mbsyncrc AAA")
                               )

                      )
                    ,(make-mu4e-context
                      :name "BBB"
                      :enter-func (lambda () (mu4e-message "Enter BBB"))
                      :leave-func (lambda () (mu4e-message "Leave BBB"))
                      ;; we match based on the contact-fields of the message
                      :match-func (lambda (msg)
                                    (when msg
                                      (mu4e-message-contact-field-matches
                                       msg
                                       :to "BBB@xxx.com")))
                      :vars '( (user-mail-address . "BBB@xxx.com")
                               (user-full-name	    . "BBB yyy")
                               (mu4e-compose-signature .
                                                        (concat
                                                         "Best Regards,\n"
                                                         "BBB yyy\n"))
                               (mu4e-inbox-folder ."/BBB/Inbox")
                               (mu4e-drafts-folder . "/BBB/[Gmail]/Drafts")
                               (mu4e-sent-folder .   "/BBB/[Gmail]/Sent Mail")
                               (mu4e-trash-folder .  "/BBB/[Gmail]/Trash")

                               (mu4e-maildir-shortcuts . (("/BBB/Inbox" . ?i)
                                                          ("/BBB/[Gmail]/Sent Mail"  . ?s)
                                                          ("/BBB/[Gmail]/Trash" . ?t)
                                                          ))


                               (mu4e-get-mail-command . "mbsync -c ~/.emacs.d/applications/email/.mbsyncrc BBB")

                               (smtpmail-default-smtp-server . "smtp.gmail.com")
                               (smtpmail-local-domain . "smtp.gmail.com")
                               (smtpmail-smtp-user . "BBB@xxx.com")
                               (smtpmail-smtp-server . "smtp.gmail.com")
                               (smtpmail-stream-type . starttls)
                               (smtpmail-smtp-service . 587)

                               )

                      )
                    )

                 mu4e-context-policy 'pick-first
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

(defun dot-offlineimap ()
  (interactive)
  (comint-send-file
   (in-emacs-d "applications/email/.offlineimaprc")
   "~/.offlineimaprc")
  (comint-send-file
   (in-emacs-d "applications/email/.offlineimaprc.py")
   "~/.offlineimaprc.py")
  )
