
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
 (c++-mode . ((tab-width . 4)
              (c-basic-offset . 4)
              (indent-tabs-mode . nil)))
 (c-mode . ((tab-width . 4)
            (c-basic-offset . 4)
            (indent-tabs-mode . nil))))
"))


;;(require 'judge-indent)
;;(global-judge-indent-mode t)
;;(setq judge-indent-major-modes '(c-mode python-mode sh-mode))
;;(setq judge-indent-default-indent-width 2)
;;(setq judge-indent-default-tab-width 8)
;;(setq judge-indent-prefer-tabs-mode nil)
