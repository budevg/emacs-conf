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
                     :models '(gemini-2.0-flash
                               gemini-2.0-pro-exp
                               gemini-2.0-flash-thinking-exp
                               ))

  (gptel-make-openai "github"
                     :host "models.inference.ai.azure.com"
                     :endpoint "/chat/completions"
                     :key 'gptel-api-key
                     :stream t
                     :models '(gpt-4o
                               gpt-4o-mini
                               DeepSeek-R1
                               ai21-jamba-1.5-large
                               ai21-jamba-1.5-mini
                               Codestral-2501
                               Mistral-large
                               Mistral-small))

  (gptel-make-openai "nebius.ai"
                     :host "api.studio.nebius.ai"
                     :endpoint "/v1/chat/completions"
                     :key 'gptel-api-key
                     :stream t
                     :models '(deepseek-ai/DeepSeek-R1
                               deepseek-ai/DeepSeek-Coder-V2-Lite-Instruct))

  (setq gptel-backend (cdr (assoc "gemini" gptel--known-backends #'equal))
        gptel-model 'gemini-2.0-flash
        gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (kernel . "You are an expert in Linux development and low-level system programming, deeply familiar with the Linux kernel internals and user-space interactions. Answer questions with detailed technical explanations.")
          (nvidia . "You are an expert in NVIDIA AI GPUs, DPUs, and low-level programming for accelerated computing. Answer my questions with detailed technical accuracy, referencing specific architectural features and low-level programming techniques where relevant. Assume I have a strong understanding of computer architecture and parallel programming concepts.")
          ))
  )
