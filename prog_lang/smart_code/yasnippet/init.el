
(defun load-yasnippet ()
  (setq yas-snippet-dirs (in-emacs-d "prog_lang/smart_code/yasnippet/snippets"))
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
  )

(run-with-idle-timer 2 nil 'load-yasnippet)
