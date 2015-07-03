(eval-after-load "asm-mode"
  '(progn
     (define-key asm-mode-map [tab] 'asm-indent-line)
     (setq asm-comment-char ?\/)
     (add-hook 'asm-mode-set-comment-hook
               (lambda ()
                 (set (make-local-variable 'comment-padding) " ")
                 (set (make-local-variable 'tab-always-indent) t)
                 (set (make-local-variable 'tab-width) 4)
                 (set (make-local-variable 'tab-stop-list) '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))))

     ))

(autoload 'nasm-mode "nasm-mode" nil t)


(defun disas-64 (start end)
  (interactive "r")
  (disas-generic start end "64"))

(defun disas-32 (start end)
  (interactive "r")
  (disas-generic start end "32"))

(defun disas-generic (start end mode)
  (let ((out-buffer (concat "*disas-" mode "*"))
        (disas-script (in-emacs-d "prog_lang/assembly/disas.py")))
    (if (get-buffer out-buffer)
        (kill-buffer out-buffer))
    (call-process-region
     start end
     disas-script      ; Name of program.
     nil               ; Do not delete region.
     out-buffer        ; Send output to buffer.
     nil               ; No redisplay during output.
     mode)             ; params
    (switch-to-buffer-other-window out-buffer)
    ))
