;; Valuable keybindings:

;; Action on client                Result in Gmail on the web         Command in Gnus
;; ----------------                --------------------------         --------------
;; Refresh inbox                   Refresh                            M-g
;; Open a message                  Mark a message as read             RET
;; Flag a message                  Apply a star to the message        !
;; Unflag a message                Remove the star from the message   M-u
;; Move a message to a folder      Apply a label to the messageB      m
;; Create a folder                 Create a label                     B m to nonexistent folder will create it.
;; Move a message to [Gmail]/Spam  Report a message as spam           B m [Gmail]Spam RET
;; Move a message to [Gmail]/Trash Move a message to Trash            B m [Gmail]Trash RET
;; Send a message                  Store message in Sent Mail         m
;; Delete a message in inbox       Remove the message from inbox      B DEL
;; Delete a message from a folder  Remove that label from the message B DEL

(setq gnus-select-method '(nntp "news.gmane.org"))
(defun gmail-address (name mail)
  (add-to-list 'gnus-secondary-select-methods
               `(nnimap ,name
                        (nnimap-user ,mail)
                        (nnimap-address "imap.gmail.com")
                        (nnimap-server-port 993)
                        (nnimap-stream ssl))))

(gmail-address "gmail" "user@gmail.com")

(setq

 gnus-save-newsrc-file t
 gnus-read-newsrc-file t
 gnus-always-read-dribble-file t
 gnus-always-save-dribble-file t
 gnus-inhibit-startup-message t
 gnus-play-startup-jingle nil
 gnus-interactive-exit nil
 gnus-treat-body-boundary nil
 gnus-treat-buttonize t
 gnus-treat-buttonize-head 'head
 gnus-treat-date-original 'head
 gnus-treat-display-smileys t
 gnus-treat-emphasize t
 gnus-treat-fill-long-lines nil
 gnus-treat-from-picon 'head
 gnus-treat-hide-boring-headers 'head
 gnus-treat-highlight-citation t
 gnus-treat-highlight-headers 'head
 gnus-treat-highlight-signature 'last
 gnus-treat-strip-cr t
 gnus-treat-strip-leading-blank-lines t
 gnus-treat-strip-trailing-blank-lines t
 gnus-treat-strip-pem t
 gnus-treat-translate t
 gnus-auto-select-first nil
 gnus-summary-same-subject ""
 ;;gnus-dribble-directory "c:/temp"
 ;;gnus-newsgroup-maximum-articles 300

 mm-discouraged-alternatives '("text/html" "text/richtext")

 gnus-check-new-newsgroups nil
 gnus-ignored-newsgroups ""

 nnml-directory "~/.gmail"
 message-directory "~/.gmail"

 gnus-user-date-format-alist '((t . "%d/%m/%Y %H:%M"))
 gnus-group-line-format "%P%M%S\t%(%g%) (%y)\n"
 gnus-summary-line-format "%1{%U%R%z: %}%2{%&user-date;%}%5{ %[%5k%] %}%(%uj %4{%-24,24n%}%6{%-4,4ur%}%5{│ %}%1{%B%}%s%)\n"
 gnus-server-line-format "   {%(%h:%w%)} %s - %n\n"

 ;; articles sorting by date (reverse)
 gnus-thread-sort-functions '(lambda (t1 t2) (not (gnus-thread-sort-by-date t1 t2)))

 gnus-sum-thread-tree-root "● "
 gnus-sum-thread-tree-false-root "▷ "
 gnus-sum-thread-tree-single-indent ""
 gnus-sum-thread-tree-leaf-with-other "├─►"
 gnus-sum-thread-tree-vertical "│ "
 gnus-sum-thread-tree-single-leaf "└─►"

 )

(defun my-gnus-group-list-subscribed-groups ()
  "List all subscribed groups with or without un-read messages"
  (interactive)
  (gnus-group-list-all-groups 5))

(add-hook 'gnus-group-mode-hook
          ;; list all the subscribed groups even they contain zero un-read messages
          (lambda () (local-set-key "o" 'my-gnus-group-list-subscribed-groups )))


;; Labels

(defun rs-gnus-summary-limit-to-label (regexp &optional not-matching)
  "Limit the summary buffer to articles that match a label."
  (interactive
   (list (read-string
	  (format "%s label (regexp): "
		  (if current-prefix-arg "Exclude" "Limit to")))
         current-prefix-arg))
  (gnus-summary-limit-to-extra 'X-Label regexp not-matching))

(defun rs-gnus-get-label (header)
  "Returns label from X-Label header"
  (let ((lbl (or (cdr (assq 'X-Label (mail-header-extra header))) ""))) lbl))

(defalias 'gnus-user-format-function-r 'rs-gnus-get-label)

;; Where am I mentioned in To and CC?
;; http://emacs.wordpress.com/category/gnus/

(defun gnus-user-format-function-j (headers)
  (let ((to (gnus-extra-header 'To headers)))
    (if (string-match *user-mails* to)
        (if (string-match "," to) "»" " ")
        (if (or (string-match *user-mails*
                              (gnus-extra-header 'Cc headers))
                (string-match *user-mails*
                              (gnus-extra-header 'BCc headers)))
            "~"
            " "))))
;; define own faces

(copy-face 'default 'face-1)
(set-face-font 'face-1 "-outline-DejaVu Sans Mono-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-2")

(copy-face 'italic 'face-4)
(set-face-foreground 'face-4 "orange")

(copy-face 'face-1 'face-5)
(set-face-foreground 'face-5 "grey50")

(copy-face 'face-1 'face-6)
(set-face-foreground 'face-6 "red")

(setq gnus-face-1 'face-1)
(setq gnus-face-4 'face-4)
(setq gnus-face-5 'face-5)
(setq gnus-face-6 'face-6)

;;
;; useful hooks
;;

(add-hook 'message-mode-hook
          '(lambda()
             (flyspell-mode t)
             (turn-on-auto-fill)
             (setq fill-column 80)))

(add-hook 'gnus-summary-mode-hook 'hl-line-mode)
(add-hook 'gnus-group-mode-hook 'hl-line-mode)

;; put groups into topics
(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

;; increase score of own articles and follow-ups
(add-hook 'message-sent-hook 'gnus-score-followup-thread) ;;'gnus-score-followup-article
