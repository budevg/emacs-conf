;;; Autoloads for magit

(autoload 'magit-status "magit" nil t)
(setq magit-save-some-buffers nil)
(global-set-key [(meta m)] 'magit-status)
