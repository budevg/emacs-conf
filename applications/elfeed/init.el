(use-package elfeed-summary
  :commands (elfeed-summary)
  :config
  (setq elfeed-summary-default-filter ""
        elfeed-summary--only-unread t
        elfeed-summary-settings
        '((auto-tags (:original-order . t)
                     )
          )))


(use-package elfeed
  :after (elfeed-summary)

  :custom
  (elfeed-search-title-max-width 120)

  :config
  (defun elfeed-show-url-in-eww (url &optional arg)
    (with-current-buffer (generate-new-buffer "*feed*")
      (eww-mode)
      (eww url)))

  (defun elfeed-show-eww-open ()
    (interactive)
    (let ((browse-url-browser-function #'elfeed-show-url-in-eww))
      (elfeed-show-visit nil)))

  (defun elfeed-search-eww-open ()
    (interactive)
    (let ((browse-url-browser-function #'elfeed-show-url-in-eww))
      (elfeed-search-browse-url nil)))
  7
  (define-key elfeed-show-mode-map (kbd "/") 'elfeed-show-eww-open)
  (define-key elfeed-search-mode-map (kbd "/") 'elfeed-search-eww-open)
  (define-key elfeed-show-mode-map (kbd "C-/") 'elfeed-show-visit)
  (define-key elfeed-search-mode-map (kbd "C-/") 'elfeed-search-browse-url)

  (mapcar
   (lambda (e)
     (let ((k (car e))
           (f (cdr e)))
       (define-key elfeed-search-mode-map (kbd k)
                   `(lambda () (interactive) (elfeed-search-set-filter ,f)))
       ))
   '(("1" . "@3-days-ago +unread")
     ("2" . "@1-weeks-ago +unread")
     ("3" . "@1-mongths-ago +unread")
     ("4" . "+unread"))
   )

  (defun elfeed-custom-sort (a b)
    (let* ((a-tags (format "%s" (elfeed-entry-tags a)))
           (b-tags (format "%s" (elfeed-entry-tags b)))
           (a-title (elfeed-feed-title (elfeed-entry-feed a)))
           (b-title (elfeed-feed-title (elfeed-entry-feed b))))
      (if (string= a-tags b-tags)
          (if (string= a-title b-title)
              (< (elfeed-entry-date b) (elfeed-entry-date a))
            (string< b-title a-title))
        (string< a-tags b-tags))))

  (defun elfeed-entry-reformat (buff)
    (switch-to-buffer buff)
    (writing-mode 1)
    (elfeed-show-refresh))

  (setf elfeed-search-sort-function #'elfeed-custom-sort)
  (setq elfeed-show-entry-switch 'elfeed-entry-reformat
        elfeed-search-remain-on-entry t))

(use-package elfeed-org
  :after (elfeed)
  :config
  (elfeed-org))
