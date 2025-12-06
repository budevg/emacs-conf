
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function kill-buffer-query-functions))

(use-package shell
  :defer t
  :config
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

  :bind (("<f2>" . shell)
         ("C-<f2>" . new-shell)
         ("M-<f2>" . shell-command-on-region-inplace)
         )
  )

(use-package comint
  :defer t
  :config
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

  (ansi-color-for-comint-mode-on)

  :bind (:map comint-mode-map
         ("M-p" . nil)
         ("C-<up>" . comint-previous-matching-input-from-input)
         ("C-<down>" . comint-next-matching-input-from-input)
         ("C-c C-k" . comint-clear-buffer)
         )
  )

(use-package vterm
  :commands (vterm)
  :config
  (defun new-vterm ()
    (interactive)
    (let ((buf (generate-new-buffer "*vterm*")))
      (pop-to-buffer-same-window buf)
      (with-current-buffer buf
        (unless (derived-mode-p 'vterm-mode)
          (vterm-mode)))
      ))
  (defun vterm-cua-paste (&optional arg)
    (interactive "P")
    (vterm-goto-char (point))
    (let ((inhibit-read-only t)
          (buffer-read-only nil))
      (cl-letf (((symbol-function 'insert-for-yank) #'vterm-insert))
        (cua-paste arg))))
  (define-key vterm-mode-map (kbd "S-<insert>") #'vterm-cua-paste)
  )

(use-package eat
  :commands (eat)
  :custom
  (eat-term-name "xterm-256color")
  (eat-kill-buffer-on-exit t)
  (eat-enable-shell-prompt-annotation nil)
  :bind (("<f3>" . eat)
         ("C-<f3>" . (lambda() (interactive) (eat nil t)))
         :map eat-line-mode-map
         ("C-c C-l" . eat-cycle-modes)
         :map eat-mode-map
         ("C-c C-l" . eat-cycle-modes)
         )
  :config
  (defun eat-cycle-modes ()
    (interactive)
    (cond
     (eat--line-mode
      (eat-semi-char-mode)
      (when (and (boundp 'cursor-type-modified) cursor-type-modified)
        (setq cursor-type nil)
        (kill-local-variable 'cursor-type-modified)))
     (t
      (eat-line-mode)
      (when (not cursor-type)
        (make-local-variable 'cursor-type-modified)
        (setq cursor-type-modified t
              cursor-type 'box)))))
  )

(defun dot-dircolors ()
  (interactive)
  (comint-send-file
   (in-emacs-d "shell/.dircolors")
   "~/.dircolors"))
