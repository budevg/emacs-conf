(use-package mpv
  :commands (mpv-play mpv-play-url)
  )

(use-package yeetube
  :commands (yeetube-search)
  :hook (yeetube-mode . (lambda ()
                         (setq-local cua-mode nil
                                     cua--keymap-alist nil)))
  :config
  (setq yeetube-download-directory (getenv "TMPDIR")
        yeetube-invidious-instances '("inv.nadeko.net")
        yeetube-default-sort-column "Date"
        )
  (add-to-list 'yeetube-mpv-additional-flags "--msg-level=all=error")

  (defun yeetube-copy-url ()
    "Copy URL for video at point"
    (interactive)
    (let ((url (yeetube-get-url)))
      (message "%s" url)
      (kill-new url)))

  (defun yeetube-browse-youtube-url ()
    (interactive)
    (browse-url (yeetube-get-url)))

  (defun yeetube-download-and-play ()
    "Download the video at point using the Q quality setting, then play it."
    (interactive)
    (unless (eq (plist-get (yeetube--find-item (tabulated-list-get-id)) :type)
                'video)
      (user-error "Select a video first"))
    (message "Downloading %s -> %s"
             (plist-get (yeetube--find-item (tabulated-list-get-id)) :title)
             (abbreviate-file-name yeetube-download-directory))
    ;; ponytail: yt-dlp skips existing downloads and reports the final path.
    (make-process
     :name "yeetube-download"
     :buffer (generate-new-buffer "*yeetube-download*")
     :stderr "*yeetube-download-errors*"
     :command (append
               (when yeetube-enable-tor (list yeetube-torsocks-program))
               (list (or yeetube-ytdlp-program "yt-dlp")
                     "-f" (format "bestvideo[height<=?%s]+bestaudio/best"
                                  yeetube-mpv-video-quality)
                     "--no-playlist" "--no-simulate"
                     "--progress" "--newline" "--progress-delta" "1"
                     "--print" "after_move:filepath"
                     "-o" (expand-file-name "%(title)s [%(id)s].%(ext)s"
                                            yeetube-download-directory)
                     (yeetube-get-url)))
     :noquery t
     :filter
     (lambda (process output)
       (internal-default-process-filter process output)
       (with-current-buffer (process-buffer process)
         (save-excursion
           (goto-char (point-max))
           (when (re-search-backward "^\\[download\\].*\n" nil t)
             (let ((message-log-max nil))
               (unless (active-minibuffer-window)
                 (message "%s" (string-trim (match-string-no-properties 0)))))))))
     :sentinel
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (unwind-protect
             (if (and (eq (process-status process) 'exit)
                      (zerop (process-exit-status process)))
                 (progn
                   (funcall yeetube-play-function
                            (with-current-buffer (process-buffer process)
                              (goto-char (point-max))
                              (skip-chars-backward "\n\r")
                              (buffer-substring-no-properties
                               (line-beginning-position) (point))))
                   (message nil))
               (message "Download failed; see *yeetube-download-errors*"))
           (kill-buffer (process-buffer process)))))))

  (defun yeetube-delete-downloaded-video ()
    "Delete local downloads of the video at point."
    (interactive)
    (let* ((id (tabulated-list-get-id))
           (item (and id (yeetube--find-item id))))
      (unless (eq (plist-get item :type) 'video)
        (user-error "Select a video first"))
      (let ((files
             (when (file-directory-p yeetube-download-directory)
               (cl-remove-if-not
                #'file-regular-p
                (directory-files
                 yeetube-download-directory t
                 (concat "\\[" (regexp-quote id) "\\]\\."
                         (regexp-opt '("mp4" "mkv" "webm" "mov" "m4v" "flv" "avi"))
                         "\\'"))))))
        (if files
            (dolist (file files)
              (delete-file file)
              (message "Deleted %s" (abbreviate-file-name file)))
          (message "No download found: %s" (plist-get item :title))))))

  :bind (("C-f v v" . yeetube-search)
         :map yeetube-mode-map
         ("C-<return>" . yeetube-download-and-play)
         ("M-<return>" . yeetube-delete-downloaded-video)
         ("w" . yeetube-copy-url)
         ("/" . yeetube-browse-youtube-url)
         ("]" . tabulated-list-sort)
         ("<tab>" . yeetube-ui-cycle-column)
         ("<backtab>" . (lambda () (interactive) (yeetube-ui-cycle-column t)))
         )
  )
