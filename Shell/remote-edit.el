(require 'comint-redirect)

(defun remote-edit-commit ()
  (interactive)
  (let ((remote-file-name remote-file-name-loc)
        (modified-buffer-string (buffer-string)))
    (with-current-buffer remote-comint-buffer-loc
      (end-of-buffer)
      (delete-region (line-beginning-position) (line-end-position))
      ;;(comint-send-input)
      (insert (format "cat > %s" remote-file-name))
      (comint-send-input)
      (insert modified-buffer-string)
      (comint-send-eof))
    (remote-edit-quit)))

(defun remote-edit-quit ()
  (interactive)
  (set-buffer-modified-p nil)
  (let ((remote-buffer remote-comint-buffer-loc))
    (kill-this-buffer)
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

