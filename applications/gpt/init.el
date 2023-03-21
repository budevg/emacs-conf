(autoload 'gptel "gptel" nil t)
(autoload 'gptel-curl-get-response "gptel-curl" nil t)
(eval-after-load "gptel"
  '(progn
     (setq gptel-default-mode 'markdown-mode
           )
     (define-key gptel-mode-map (kbd "C-c C-c") #'gptel-send))
  )
