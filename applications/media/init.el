(use-package mpv
  :commands (mpv-play mpv-play-url)
  )

(use-package yeetube
  :commands (yeetube-search)
  :config
  (setq yeetube-download-directory (getenv "TMPDIR")
        yeetube-invidious-instances '("inv.nadeko.net")
        yeetube-default-sort-column "Date"
        )

  (defun yeetube-copy-url ()
    "Copy URL for video at point"
    (interactive)
    (let ((url (yeetube-get-url)))
      (message "%s" url)
      (kill-new url)))

  (defun yeetube-browse-youtube-url ()
    (interactive)
    (browse-url (yeetube-get-url)))

  (define-key yeetube-mode-map (kbd "w") #'yeetube-copy-url)
  (define-key yeetube-mode-map (kbd "/") #'yeetube-browse-youtube-url)

  :bind (("C-f v v" . yeetube-search))
  )
