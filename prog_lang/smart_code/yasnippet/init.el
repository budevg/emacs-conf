
(defun load-yasnippet ()
  (setq yas-snippet-dirs
        (list (in-emacs-d "prog_lang/smart_code/yasnippet/snippets")))
  (setq yas-prompt-functions '(yas-x-prompt
                               yas-dropdown-prompt
                               yas-completing-prompt
                               yas-maybe-ido-prompt
                               yas-no-prompt))
  (setq yas-verbosity 0)
  (require 'yasnippet)
  (yas-global-mode 1)
  (defun iy-ac-tab-noconflict ()
    (interactive)
    (let ((command (key-binding [tab]))) ; remember command
      (if command
          (progn
            (local-unset-key [tab]) ; unset from (kbd "<tab>")
            (local-set-key (kbd "TAB") command))))) ; bind to (kbd "TAB")
  (add-hook 'ruby-mode-hook 'iy-ac-tab-noconflict)
  (add-hook 'markdown-mode-hook 'iy-ac-tab-noconflict)
  (add-hook 'org-mode-hook 'iy-ac-tab-noconflict)
  (setq yas-prompt-functions '(yas-ido-prompt))
  )

(run-with-idle-timer 2 nil 'load-yasnippet)

(use-package yankpad
  :after (yasnippet)
  :init
  (setq yankpad-file (in-emacs-d "prog_lang/smart_code/yasnippet/snippets/yankpad.org"))
  :bind (("M-1" . yankpad-insert)
         ("M-!" . yankpad-set-category))
  )
