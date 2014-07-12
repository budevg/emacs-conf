
(setq-default indent-tabs-mode nil)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
(show-paren-mode)

(defun create-dir-locals ()
  (interactive)
  (find-file dir-locals-file)
  (insert "
((nil . ((tab-width . 4)
         (tab-stop-list . (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))))
 (asm-mode . ((comment-start . \"/\")))
 (c++-mode . ((tab-width . 4)
              (c-basic-offset . 4)
              (indent-tabs-mode . nil)))
 (c-mode . ((tab-width . 4)
            (c-basic-offset . 4)
            (indent-tabs-mode . nil))))
"))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(add-hook 'prog-mode-hook '(lambda ()
                             (local-set-key (kbd "M-o") 'smart-open-line)
                             (local-set-key (kbd "M-O") 'smart-open-line-above)))
