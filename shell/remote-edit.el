(defun remote-edit-commit ()
  (interactive)
  (let ((remote-file-name remote-file-name-loc)
        (modified-buffer-string (buffer-string)))
    (with-current-buffer remote-comint-buffer-loc
      (end-of-buffer)
      (delete-region (line-beginning-position) (line-end-position))
      (insert (format "cat << \"EOF\" > %s\n" remote-file-name))
      (insert modified-buffer-string)
      (insert "\nEOF\n")
      (comint-send-input)
      )
    (remote-edit-quit)))

(defun remote-edit-quit ()
  (interactive)
  (set-buffer-modified-p nil)
  (let ((remote-buffer remote-comint-buffer-loc))
    (kill-current-buffer)
    (switch-to-buffer remote-buffer)))


(defun remote-edit-file ()
  (interactive)
  (if (comint-check-proc (current-buffer))
      (let* ((remote-file-name (thing-at-point 'filename))
             (remote-comint-buffer (current-buffer))
             (output-buffer (get-buffer-create (concat "___" (file-name-nondirectory remote-file-name))))
             )
        (comint-redirect-send-command (format "cat %s" remote-file-name) output-buffer nil)

        (with-current-buffer output-buffer
          (setq buffer-file-name (file-name-nondirectory remote-file-name))
          (set-auto-mode)
          (set (make-local-variable 'remote-file-name-loc) remote-file-name)
          (set (make-local-variable 'remote-comint-buffer-loc) remote-comint-buffer)

          (local-set-key [(control c) (control a)] 'remote-edit-quit)
          (local-set-key [(control c) (control c)] 'remote-edit-commit)
          ))))

(use-package tramp
  :defer t
  :init
  (setq remote-file-name-inhibit-locks t
        tramp-use-scp-direct-remote-copying t
        tramp-copy-size-limit 1000000
        tramp-verbose 2
        remote-file-name-inhibit-auto-save-visited t))

  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)
