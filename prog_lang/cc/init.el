(use-package cc-mode
  :mode (("\\.cu[h]?\\'" . c++-mode))

  :config
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))

  (c-add-style
   "linux-tabs-only"
   '("linux" (c-offsets-alist
              (arglist-cont-nonempty
               c-lineup-gcc-asm-reg
               c-lineup-arglist-tabs-only))))

  (defun default-c-mode-hook ()
    (c-set-style "linux")
    (setq c-basic-offset 4
          tab-width 4
          indent-tabs-mode nil)
    (font-lock-add-keywords
     nil
     '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))

  :hook
  ((c-mode-common-hook . default-c-mode-hook)
   (c++-mode-common-hook . default-c-mode-hook))

  :bind (:map c-mode-base-map
         ("C-d" . nil)
         ("C-M-o" . (lambda ()
                      (interactive)
                      (let ((inhibit-message t)
                            (ff-always-try-to-create nil)
                            (ff-ignore-include t))
                        (ff-find-other-file nil t))))
         :map c-mode-map
         ("C-m" . newline-and-indent)
         :map c++-mode-map
         ("C-m" . newline-and-indent)
         )
)
