
(defun dot-gdb-crack ()
  (interactive)
  (comint-send-file
   (in-emacs-d "prog_lang/debug/.gdbinit-crack")
   "~/.gdbinit"))

(defun dot-gdb-common ()
  (interactive)
  (comint-send-file
   (in-emacs-d "prog_lang/debug/.gdbinit-common")
   "~/.gdbinit"))



(eval-after-load "gud"
  '(progn
     (add-hook 'gud-mode-hook
               '(lambda ()
                  (local-set-key [control up]          ; cycle backward through command history
                                 '(lambda () (interactive)
                                    (if (comint-after-pmark-p)
                                        (comint-previous-input 1)
                                      (previous-line 1))))
                  (local-set-key [control down]        ; cycle forward through command history
                                 '(lambda () (interactive)
                                    (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                   (forward-line 1))))
                  (local-set-key [kp-2] 'gud-step)
                  (local-set-key [kp-5] 'gud-break)
                  (local-set-key [kp-8] 'gud-finish)
                  (local-set-key [kp-6] 'gud-next)
                  (local-set-key [kp-7] 'gud-up)
                  (local-set-key [kp-1] 'gud-down)

                  ))))
