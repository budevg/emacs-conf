
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(defun new-shell ()
  "create new shell"
  (interactive)
  (shell (generate-new-buffer "*shell*"))
)

(defun shell-command-on-region-inplace ()
  (interactive)
  (let ((command (read-shell-command "Shell command on region: ")))
    (shell-command-on-region (region-beginning) (region-end) command 1)))

(defun shell-change-to-current-dir ()
  (interactive)
  (let ((current-directory-path (expand-file-name default-directory)))
    (shell)
    (end-of-buffer)
    (insert-string "cd \"" )
    (insert-string current-directory-path)
    (insert-string "\"" )
    (comint-send-input)))



(global-set-key [(f2)] 'shell)
(global-set-key [(meta \\)] 'shell-change-to-current-dir)
(global-set-key [(control f2)] 'new-shell)
(global-set-key [(meta f2)] 'shell-command-on-region-inplace)

(eval-after-load "comint"
  '(progn
     (ansi-color-for-comint-mode-on)
     (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)))


(autoload 'multi-term "multi-term" nil t)
(eval-after-load "term"
  '(progn
     (add-hook 'term-mode-hook
               (lambda ()
                 ;; used to allow persisten highlighting in term mode
                 ;; not sure if this is bug in emacs-23.2.91 pretest
                 (set (make-local-variable 'font-lock-fontified) t)))))
(global-set-key [f3] 'multi-term)

(autoload 'cssh-term-remote-open "cssh" nil t)
(global-set-key (kbd "C-=") 'cssh-term-remote-open)