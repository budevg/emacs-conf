(use-package compile
  :bind (("<f8>" . compile))
  )

(use-package newcomment
  :bind (("M-3" . my-comment-region)
         ("M-4" . comment-box)
         )
  :config

  (defun my-comment-region ()
    (interactive)
    (let ((beg (if mark-active (region-beginning) (point-at-bol)))
	  (end (if mark-active (region-end) (point-at-eol))))
      (comment-or-uncomment-region beg end)))

  (setq comment-padding 0)

  )

(setq-default indent-tabs-mode nil)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)
(show-paren-mode)

(defun create-dir-locals ()
  (interactive)
  (find-file dir-locals-file)
  (insert-file-contents (in-emacs-d "prog_lang/dir-locals.el"))
  )

(use-package prog-mode
  :bind ((:map prog-mode-map
               ("M-o" . smart-open-line)
               ("M-O" . smart-open-line-above)))
  :config
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
  )
