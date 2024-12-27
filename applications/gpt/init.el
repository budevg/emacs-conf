(use-package gptel
  :commands (gptel gptel-send gptel-menu)
  :bind (("C-f g" . gptel-send)
         ("C-f G" . gptel-menu)
         :map gptel-mode-map
         ("C-c C-c" . gptel-send)
         ("C-c C-e" . gptel-menu))
  :config
  (require 'gptel-curl)
  (require 'gptel-gemini)
  (require 'gptel-openai)
  (require 'gptel-transient)
  (require 'gptel-context)
  (require 'gptel-rewrite)
  (require 'diff)

  (setq gptel--known-backends nil)

  (gptel-make-gemini "gemini"
                     :key 'gptel-api-key
                     :stream t
                     :models '(gemini-1.5-pro
                               gemini-1.5-flash
                               gemini-1.5-flash-8b
                               learnlm-1.5-pro-experimental
                               gemini-2.0-flash-exp))

  (gptel-make-openai "github"
                     :host "models.inference.ai.azure.com"
                     :endpoint "/chat/completions"
                     :key 'gptel-api-key
                     :stream t
                     :models '(gpt-4o
                               gpt-4o-mini
                               ai21-jamba-1.5-large
                               ai21-jamba-1.5-mini
                               Mistral-large
                               Mistral-small))

  (gptel-make-openai "nebius.ai"
                     :host "api.studio.nebius.ai"
                     :endpoint "/v1/chat/completions"
                     :key 'gptel-api-key
                     :stream t
                     :models '(meta-llama/Meta-Llama-3.1-8B-Instruct
                               meta-llama/Meta-Llama-3.1-70B-Instruct
                               meta-llama/Meta-Llama-3.1-405B-Instruct
                               mistralai/Mistral-Nemo-Instruct-2407
                               mistralai/Mixtral-8x7B-Instruct-v0.1
                               mistralai/Mixtral-8x22B-Instruct-v0.1
                               deepseek-ai/DeepSeek-Coder-V2-Lite-Instruct
                               microsoft/Phi-3-mini-4k-instruct
                               allenai/OLMo-7B-Instruct-hf))

  (setq gptel-backend (cdr (assoc "gemini" gptel--known-backends #'equal))
        gptel-model 'gemini-2.0-flash-exp
        gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          ))
  )
