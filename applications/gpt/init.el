(use-package gptel
  :commands (gptel gptel-send gptel-menu)
  :bind (("C-f g" . gptel-send)
         ("C-f G" . gptel-menu))
  :config
  (require 'gptel-curl)
  (require 'gptel-gemini)
  (require 'gptel-transient)
  (require 'gptel-context)
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
