
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

(defun dot-dircolors ()
  (interactive)
  (comint-send-file
   (in-emacs-d "shell/.dircolors")
   "~/.dircolors"))
