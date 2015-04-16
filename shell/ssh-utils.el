(defun ssh-authorize ()
  (interactive)
  (end-of-buffer)
  (insert-string "cat <<EOF >>~/.ssh/authorized_keys")
  (comint-send-input)
  (insert-file-contents "~/.ssh/id_rsa.pub")
  (comint-send-input)
  (insert-string "EOF")
  (comint-send-input))

(autoload 'remote-edit-file "remote-edit" nil t)
(define-key ctl-x-map "F" 'remote-edit-file)

(defun dot-ftrace ()
  (interactive)
  (let* ((util (ido-completing-read
                "Ftrace utility: " '("execsnoop"
                                     "funccount"
                                     "funcgraph"
                                     "funcslower"
                                     "functrace"
                                     "iolatency"
                                     "iosnoop"
                                     "kprobe"
                                     "tpoint")))
         (src (concat (in-emacs-d "shell/ftrace/") util))
         (dst util))
    (comint-send-file-base64 src dst "+x")))
