
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(add-to-list 'display-buffer-alist
             '("^\\*shell\\*" . (display-buffer-same-window)))

(defun new-shell ()
  "create new shell"
  (interactive)
  (shell (generate-new-buffer "*shell*"))
)

(defun shell-command-on-region-inplace ()
  (interactive)
  (let ((command (read-shell-command "Shell command on region: ")))
    (shell-command-on-region (region-beginning) (region-end) command t t)))

(defun shell-change-to-current-dir ()
  (interactive)
  (let ((current-directory-path (expand-file-name default-directory)))
    (shell)
    (end-of-buffer)
    (insert "cd \"" )
    (insert current-directory-path)
    (insert "\"" )
    (comint-send-input)))

(global-set-key [(f2)] 'shell)
(global-set-key [(control f2)] 'new-shell)
(global-set-key [(meta f2)] 'shell-command-on-region-inplace)

(eval-after-load "comint"
  '(progn
     ;;["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"]
     ;; change blue->yellow
     (defun comint-previous-matching-input-from-input (n)
       "Search backwards through input history for match for current input.
\(Previous history elements are earlier commands.)
With prefix argument N, search for Nth previous match.
If N is negative, search forwards for the -Nth following match."
       (interactive "p")
       (let ((opoint (point)))
         (unless (memq last-command '(comint-previous-matching-input-from-input
                                      comint-next-matching-input-from-input))
           ;; Starting a new search
           (setq comint-matching-input-from-input-string
                 (buffer-substring
                  (or (marker-position comint-accum-marker)
                      (process-mark (get-buffer-process (current-buffer))))
                  (point))
                 comint-input-ring-index nil))
         (comint-previous-matching-input
          (concat "^" (regexp-quote comint-matching-input-from-input-string))
          n)
         ;;(goto-char opoint)
         ))

     (setq ansi-color-names-vector ["black" "red" "green" "yellow" "yellow" "magenta" "cyan" "white"])
     (ansi-color-for-comint-mode-on)
     (define-key comint-mode-map [(meta p)] nil)
     (define-key comint-mode-map [C-up] 'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [C-down] 'comint-next-matching-input-from-input)
     (define-key comint-mode-map (kbd "C-c k") 'comint-clear-buffer))
  )


(autoload 'vterm "vterm" nil t)
(global-set-key [f3] 'vterm)
(eval-after-load "vterm"
  '(progn
     (defun vterm-cua-paste (&optional arg)
       (interactive "P")
       (vterm-goto-char (point))
       (let ((inhibit-read-only t)
             (buffer-read-only nil))
         (cl-letf (((symbol-function 'insert-for-yank) #'vterm-insert))
           (cua-paste arg))))
     (define-key vterm-mode-map (kbd "S-<insert>") #'vterm-cua-paste)
     )
  )


(defun dot-dircolors ()
  (interactive)
  (comint-send-file
   (in-emacs-d "shell/.dircolors")
   "~/.dircolors"))

(autoload 'direnv-envrc-mode "direnv" nil t)
(add-to-list 'auto-mode-alist '("\\.envrc\\'" . direnv-envrc-mode))
(autoload 'direnv-mode "direnv" nil t)
(autoload 'direnv-update-environment "direnv" nil t)

(defun nix-shell-env-load ()
  (if (not (boundp 'exec-path-orig))
      (setq exec-path-orig (copy-sequence exec-path)))
  (if (not (boundp 'process-environment-orig))
      (setq process-environment-orig (copy-sequence process-environment)))
  (let* ((direnv--executable (executable-find "nix-shell"))
         (nix-shell-path (locate-dominating-file default-directory "shell.nix"))
         (direnv--executable-args `("--run" "jq -n env"
                                    ,(expand-file-name (concat nix-shell-path "shell.nix"))))
         (direnv-always-show-summary nil))
    (direnv-update-environment))
  (message "nix shell environment was loaded"))

(defun nix-shell-env-reset ()
  (when (boundp 'exec-path-orig)
      (setq exec-path exec-path-orig)
      (makunbound 'exec-path-orig))
  (when (boundp 'process-environment-orig)
      (setq process-environment process-environment-orig)
      (makunbound 'process-environment-orig))
  (message "nix shell environment was reset"))

(defun nix-shell-env-load-or-reset ()
  (interactive)
  (if (or (boundp 'exec-path-orig) (boundp 'process-environment-orig))
      (nix-shell-env-reset)
    (nix-shell-env-load)))

(global-set-key (kbd "C-f x") 'nix-shell-env-load-or-reset)
