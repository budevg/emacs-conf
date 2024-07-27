(use-package gptel
  :commands (gptel gptel-send gptel-menu)
  :bind (("C-f g" . gptel-send)
         ("C-f G" . gptel-menu))
  :config
  (require 'gptel-curl)
  (require 'gptel-gemini)
  (require 'gptel-openai)
  (require 'gptel-transient)
  (require 'gptel-context)

  (gptel-make-gemini
     "gemini"
     :key 'gptel-api-key
     :stream t
     :models '("gemini-1.5-pro-latest"
               "gemini-1.5-flash-latest"))

  (gptel-make-openai "open-router"
  :host "openrouter.ai"
  :endpoint "/api/v1/chat/completions"
  :key 'gptel-api-key
  :stream t
  :models '("meta-llama/llama-3-8b-instruct:free"
            "google/gemma-2-9b-it:free"
            "google/gemma-7b-it:free"))

  (assoc-delete-all "ChatGPT" gptel--known-backends #'equal)

  (setq-default gptel-backend (cdr (assoc "gemini" gptel--known-backends #'equal))
                gptel-model "gemini-1.5-pro-latest")
  )
