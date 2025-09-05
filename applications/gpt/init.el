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
  (require 'gptel-openai-extras)
  (require 'gptel-transient)
  (require 'gptel-context)
  (require 'gptel-rewrite)
  (require 'gptel-integrations)
  (require 'diff)

  (setq gptel--known-backends nil)

  ;; ~/.authinfo
  ;; machine generativelanguage.googleapis.com login apikey password <KEY>
  (gptel-make-gemini "gemini"
                     :key 'gptel-api-key
                     :stream t
                     :models '(gemini-2.5-flash
                               gemini-2.5-pro
                               gemini-2.0-flash
                               ))

  ;; ~/.authinfo
  ;; machine models.github.ai login apikey password <KEY>
  (gptel-make-openai "github"
                     :host "models.github.ai"
                     :endpoint "/inference/chat/completions"
                     :key 'gptel-api-key
                     :stream t
                     :models '(gpt-4.1
                               gpt-5-mini
                               grok-3))

  ;; ~/.authinfo
  ;; machine api.studio.nebius.com login apikey password <KEY>
  (gptel-make-openai "nebius"
                     :host "api.studio.nebius.com"
                     :endpoint "/v1/chat/completions"
                     :key 'gptel-api-key
                     :stream t
                     :models '(deepseek-ai/DeepSeek-V3-0324
                               deepseek-ai/DeepSeek-R1-0528
                               Qwen/Qwen3-Coder-480B-A35B-Instruct))

  ;; ~/.authinfo
  ;; machine api.perplexity.ai login apikey password <KEY>
  (gptel-make-perplexity "perplexity"
                     :host "api.perplexity.ai"
                     :key 'gptel-api-key
                     :stream t
                     )

  (setq gptel-backend (cdr (assoc "gemini" gptel--known-backends #'equal))
        gptel-model 'gemini-2.0-flash
        gptel-directives
        '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
          (kernel . "You are an expert in Linux development and low-level system programming, deeply familiar with the Linux kernel internals and user-space interactions. Answer questions with detailed technical explanations.")
          (nvidia . "You are an expert in NVIDIA AI GPUs, DPUs, and low-level programming for accelerated computing. Answer my questions with detailed technical accuracy, referencing specific architectural features and low-level programming techniques where relevant. Assume I have a strong understanding of computer architecture and parallel programming concepts.")
          (llm . "You are a highly experienced machine learning engineer and researcher. You have a deep understanding of LLMs, statistical modeling, and deep learning. You can explain complex topics clearly and concisely. You are practical and focused on real-world applications.")
          ))
  )

(use-package mcp-hub
  :commands (mcp-hub)
  :config
  (setq mcp-hub-servers
        '(("everything" . (:command "docker" :args ("run" "--rm" "-i" "mcp/everything")))
          ))
  )
