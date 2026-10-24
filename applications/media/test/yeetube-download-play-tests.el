;;; yeetube-download-play-tests.el --- Download/play check -*- lexical-binding: t; -*-

;; emacs -Q --batch -l applications/media/test/yeetube-download-play-tests.el -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'cl-lib)
(require 'subr-x)

;; Load just the commands, without loading the user's package configuration.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../init.el" (file-name-directory load-file-name)))
  (dolist (command '(yeetube-download-and-play yeetube-delete-downloaded-video))
    (search-forward (format "(defun %s ()" command))
    (goto-char (match-beginning 0))
    (eval (read (current-buffer)))))

(defvar yeetube-enable-tor nil)
(defvar yeetube-ytdlp-program "yt-dlp")
(defvar yeetube-download-directory "/tmp/")
(defvar yeetube-play-function)
(defvar-local yeetube-mpv-video-quality nil)

(ert-deftest yeetube-delete-matches-only-complete-selected-videos ()
  (let* ((yeetube-download-directory (make-temp-file "yeetube-delete-" t))
         (names '("Old title [id].webm" "Video [id].mp4"
                  "Other [other].mp4" "Video [id].mp4.part" "Video [id].jpg"))
         echo)
    (unwind-protect
        (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () "id"))
                  ((symbol-function 'yeetube--find-item)
                   (lambda (_) '(:type video :title "Video")))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq echo (apply #'format format-string args)))))
          (dolist (name names)
            (with-temp-file (expand-file-name name yeetube-download-directory)))
          (yeetube-delete-downloaded-video)
          (should (string-prefix-p "Deleted " echo))
          (dolist (name names)
            (should (eq (file-exists-p (expand-file-name name yeetube-download-directory))
                        (not (member name (cl-subseq names 0 2))))))
          (yeetube-delete-downloaded-video)
          (should (equal echo "No download found: Video")))
      (delete-directory yeetube-download-directory t))))

(ert-deftest yeetube-download-play-on-success-only ()
  (let* (options played echo
         (yeetube-play-function (lambda (file) (setq played file))))
    (with-temp-buffer
      (let ((output (current-buffer)))
        (cl-letf (((symbol-function 'tabulated-list-get-id) (lambda () "id"))
                  ((symbol-function 'yeetube--find-item) (lambda (_) '(:type video)))
                  ((symbol-function 'yeetube-get-url) (lambda () "https://example.com"))
                  ((symbol-function 'make-process) (lambda (&rest args) (setq options args)))
                  ((symbol-function 'process-buffer) (lambda (_) output))
                  ((symbol-function 'process-status) (lambda (_) 'exit))
                  ((symbol-function 'message)
                   (lambda (format-string &rest args)
                     (setq echo (when format-string
                                  (apply #'format format-string args)))))
                  ((symbol-function 'internal-default-process-filter)
                   (lambda (_process chunk)
                     (with-current-buffer output
                       (goto-char (point-max))
                       (insert chunk))))
                  ((symbol-function 'kill-buffer) #'ignore))
          (dolist (quality '("720" "480"))
            (setq-local yeetube-mpv-video-quality quality)
            (yeetube-download-and-play)
            (should (member "--progress" (plist-get options :command)))
            (should (member "--newline" (plist-get options :command)))
            (should (equal (cadr (member "-f" (plist-get options :command)))
                           (if (equal quality "480") "bestvideo[height<=?480]+bestaudio/best"
                             "bestvideo[height<=?720]+bestaudio/best"))))
          (setq echo nil)
          (let ((filter (plist-get options :filter)))
            (funcall filter nil "[down")
            (should-not echo)
            (funcall filter nil "load] 50% ETA 00:10\n[download] 100")
            (should (equal echo "[download] 50% ETA 00:10"))
            (funcall filter nil "%\n/tmp/video with spaces.mp4\n")
            (should (equal echo "[download] 100%")))
          (dolist (status '(1 0))
            (cl-letf (((symbol-function 'process-exit-status) (lambda (_) status)))
              (funcall (plist-get options :sentinel) nil "finished")
              (if (zerop status) (should-not echo) (should echo))
              (should (equal played (and (zerop status)
                                        "/tmp/video with spaces.mp4"))))))))))
