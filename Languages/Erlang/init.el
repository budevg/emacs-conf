
(autoload 'erlang-mode "erlang" "Major mode for editing Erlang code." t)

;;
;; Associate files extensions ".erl" and ".hrl" with Erlang mode.
;;

(let ((a '("\\.erl\\'" . erlang-mode))
      (b '("\\.hrl\\'" . erlang-mode)))
  (or (assoc (car a) auto-mode-alist)
      (setq auto-mode-alist (cons a auto-mode-alist)))
  (or (assoc (car b) auto-mode-alist)
      (setq auto-mode-alist (cons b auto-mode-alist))))
