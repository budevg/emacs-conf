(autoload 'elfeed "elfeed" nil t)
(eval-after-load "elfeed"
  `(progn
     (require 'elfeed-org)
     (elfeed-org)

     (setq elfeed-search-remain-on-entry t)

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

     ))
