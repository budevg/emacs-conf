(defun comint-send-file (src-path dst-path)
  (let ((src-data (with-temp-buffer
                   (insert-file-contents src-path)
                   (buffer-string)))
        (current-proc (get-buffer-process (current-buffer))))
    (end-of-buffer)
    (comint-send-string current-proc (format "cat > %s\n" dst-path))
    (comint-send-string current-proc src-data)
    (comint-send-input nil t)
    (comint-send-input nil t)
    (comint-send-eof)
    ))

