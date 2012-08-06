(eval-after-load "asm-mode"
  '(progn
     (define-key asm-mode-map [tab] 'asm-indent-line)
     (add-hook 'asm-mode-set-comment-hook
               (lambda ()
                 (set (make-local-variable 'tab-always-indent) t)))

     ))


(defun disas-64 (start end)
  (interactive "r")
  (disas-generic start end "64"))

(defun disas-32 (start end)
  (interactive "r")
  (disas-generic start end "32"))

(defun disas-generic (start end mode)
  (let ((out-buffer (concat "*disas-" mode "*"))
        (disas-script (concat EMACS-CONFIG-PATH "/Languages/Assembly/disas.py")))
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

