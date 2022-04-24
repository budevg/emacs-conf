(autoload 'elfeed "elfeed" nil t)
(eval-after-load "elfeed"
  `(progn
     (require 'elfeed-org)
     (elfeed-org)

     (setq elfeed-search-remain-on-entry t)

     (defun elfeed-show-eww-open ()
       (interactive)
       (let ((browse-url-browser-function #'eww-browse-url))
         (elfeed-show-visit nil)))

     (defun elfeed-search-eww-open ()
       (interactive)
       (let ((browse-url-browser-function #'eww-browse-url))
         (elfeed-search-browse-url nil)))

     (define-key elfeed-show-mode-map (kbd "B") 'elfeed-show-eww-open)
     (define-key elfeed-search-mode-map (kbd "B") 'elfeed-search-eww-open)

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
