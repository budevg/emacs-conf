(use-package gptel
  :commands (gptel gptel-send)
  :bind (("C-f g" . gptel-send)
         ("C-f G" . gptel-menu))
  :config
  (autoload 'gptel-curl-get-response "gptel-curl" nil t)
  (autoload 'gptel-make-gemini "gptel-gemini" nil t)
  (autoload 'gptel-menu "gptel-transient" nil t)
  (setq gptel-gemini-backend
    (gptel-make-gemini
     "Gemini"
     :key 'gptel-api-key
     :stream t
     :models '("gemini-1.5-pro-latest" "gemini-1.5-flash-latest")
     ))
  (setq-default gptel-backend gptel-gemini-backend
                gptel-model "gemini-1.5-pro-latest")
  )
