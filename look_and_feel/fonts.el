;; themes

(setq custom-safe-themes t)
(add-to-list 'custom-theme-load-path (in-emacs-d "look_and_feel/themes"))
(load-theme 'monokai t)
(custom-set-faces '(ebrowse-root-class ((t (:foreground "yellow" :weight bold)))))
(advice-add #'shr-colorize-region :around (defun shr-no-colourise-region (&rest ignore)))

(defun what-face (pos)
    (interactive "d")
        (let ((face (or (get-char-property (point) 'read-face-name)
            (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))
