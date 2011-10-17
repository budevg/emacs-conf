
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; don't use pcomplete in shell since it tries to parse tar files for example
(eval-after-load "shell"
  '(progn
     (delq 'pcomplete-completions-at-point shell-dynamic-complete-functions)))

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
(global-set-key [(control f2)] 'new-shell)
(global-set-key [(meta f2)] 'shell-command-on-region-inplace)

(eval-after-load "comint"
  '(progn
     ;;["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
     ;; change blue->yellow
     (setq ansi-color-names-vector ["black" "red" "green" "yellow" "yellow" "magenta" "cyan" "white"])
     (ansi-color-for-comint-mode-on)
     (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)))


(autoload 'multi-term "multi-term" nil t)
(global-set-key [f3] 'multi-term)
(eval-after-load "multi-term"
  '(progn
     (setq term-bind-key-alist
           (append term-bind-key-alist
                   '(("C-<up>" . term-send-up)
                     ("<up>" . previous-line)
                     ("C-<down>" . term-send-down)
                     ("<down>" . next-line)
                     ("<right>" . forward-char)
                     ("<left>" . backward-char)
                     ("<home>" . move-beginning-of-line)
                     ("<end>" . move-end-of-line)
                     )))
       (add-hook 'term-mode-hook (lambda () (cua-mode t)))))

(autoload 'cssh-term-remote-open "cssh" nil t)
(global-set-key (kbd "C-=") 'cssh-term-remote-open)

(setq exec-path (append (list (expand-file-name "~/tools/bin")) exec-path))
