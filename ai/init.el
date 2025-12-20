(use-package gptel
  :commands (gptel gptel-send gptel-menu)
  :bind (("C-f g" . gptel-send)
         ("C-f G" . gptel-menu)
         :map gptel-mode-map
         ("C-c C-c" . gptel-send)
         ("C-c C-e" . gptel-menu)
         ("C-c C-m" . gptel-switch-model))
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

  (defun gptel-switch-model ()
    "Interactively switch the active gptel backend and model.
Uses `completing-read` to select from all available 'backend:model'
combinations."
    (interactive)
    (unless gptel--known-backends
      (user-error "No gptel backends are defined. Please configure them in your Emacs init file."))

    ;; Build a list of all "backend:model" strings with their corresponding objects
    (let* ((all-models-alist
            (cl-loop
             for (backend-name . backend-obj) in gptel--known-backends
             nconc (cl-loop for model-sym in (gptel-backend-models backend-obj)
                            collect (list (concat (gptel-backend-name backend-obj)
                                                  ":"
                                                  (gptel--model-name model-sym))
                                          backend-obj model-sym))))
           (current-selection (when (and gptel-backend gptel-model)
                                (concat (gptel-backend-name gptel-backend)
                                        ":"
                                        (gptel--model-name gptel-model))))
           (current-backend-name (when gptel-backend (gptel-backend-name gptel-backend)))
           (current-backend-models-alist nil)
           (other-models-alist nil)
           (reordered-model-names nil))

      (unless all-models-alist
        (user-error "No models found in any defined gptel backends. Please check your configuration."))

      ;; Separate models by current backend and others
      (cl-loop for entry in all-models-alist
               for combo-string = (car entry)
               if (and current-backend-name
                       (string-prefix-p (concat current-backend-name ":") combo-string))
               do (push combo-string current-backend-models-alist)
               else do (push combo-string other-models-alist))

      ;; Reorder the list: current backend models first, then others
      (setq reordered-model-names
            (append (nreverse current-backend-models-alist)
                    (nreverse other-models-alist)))

      (let* ((selected-combo
              (completing-read
               (format "Switch backend and model (current: %s): "
                       (or current-selection "None Active"))
               reordered-model-names ; Use the reordered list
               nil ; Require match
               current-selection))) ; Default value is current backend:model

        (when selected-combo
          (let* ((selected-entry (cdr (assoc selected-combo all-models-alist)))
                 (new-backend (nth 0 selected-entry))
                 (new-model (nth 1 selected-entry)))
            ;; Set buffer-locally
            (setq-local gptel-backend new-backend)
            (setq-local gptel-model new-model)
            ;; Sanitize to ensure the new model is compatible and update UI.
            ;; :shoosh nil ensures warnings are displayed if the chosen model
            ;; is not truly compatible with the backend (e.g., due to API changes),
            ;; and it will fall back to the first available model for that backend.
            (gptel--sanitize-model :backend new-backend :model new-model :shoosh nil)
            (message "Switched gptel to backend %s with model %s."
                     (gptel-backend-name gptel-backend)
                     (gptel--model-name gptel-model))
            (when gptel-mode
              (force-mode-line-update)))))))

  (setq gptel--known-backends nil
        gptel-expert-commands t)

  ;; ~/.authinfo
  ;; machine generativelanguage.googleapis.com login apikey password <KEY>
  (gptel-make-gemini "gemini"
    :key 'gptel-api-key
    :stream t
    :request-params '(:tools [(:google_search ()) (:url_context ())])
    :models '(gemini-2.5-flash-lite
              gemini-2.5-flash
              gemma-3-4b
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
  ;; machine api.tokenfactory.nebius.com login apikey password <KEY>
  (gptel-make-openai "nebius"
    :host "api.tokenfactory.nebius.com"
    :endpoint "/v1/chat/completions"
    :key 'gptel-api-key
    :stream t
    :models '(deepseek-ai/DeepSeek-V3-0324
              deepseek-ai/DeepSeek-R1-0528
              Qwen/Qwen3-Coder-480B-A35B-Instruct
              Qwen/Qwen3-Coder-30B-A3B-Instruct))

  ;; ~/.authinfo
  ;; machine api.perplexity.ai login apikey password <KEY>
  (gptel-make-perplexity "perplexity"
    :host "api.perplexity.ai"
    :key 'gptel-api-key
    :stream t
    )

  (setq gptel-backend (cdr (assoc "gemini" gptel--known-backends #'equal))
        gptel-include-reasoning nil
        gptel-model 'gemini-2.5-flash-lite
        gptel-directives
        (let* ((suffix " Also, use only plain text. Do not use Markdown, bullet points, numbering, bold, italics, or any other formatting. Output should be simple unformatted text only. Use only code blocks when necessary")
               (directives
                '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
                  (kernel . "You are an expert in Linux development and low-level system programming, deeply familiar with the Linux kernel internals and user-space interactions. Answer questions with detailed technical explanations.")
                  (nvidia . "You are an expert in NVIDIA AI GPUs, DPUs, and low-level programming for accelerated computing. Answer my questions with detailed technical accuracy, referencing specific architectural features and low-level programming techniques where relevant. Assume I have a strong understanding of computer architecture and parallel programming concepts.")
                  (llm . "You are a highly experienced machine learning engineer and researcher. You have a deep understanding of LLMs, statistical modeling, and deep learning. You can explain complex topics clearly and concisely. You are practical and focused on real-world applications.")
                  (cpp . "You are a highly experienced C++ programmer and technical expert. Your task is to provide concise, insightful, and actionable advice to another experienced C++ programmer. Focus on advanced topics, best practices, performance considerations, and modern C++ features.")
                  )))
          (mapcar (lambda (directive) (cons (car directive) (concat (cdr directive) suffix))) directives))
        )
  )

(use-package mcp-hub
  :commands (mcp-hub)
  :config
  (setq mcp-hub-servers
        '(("everything" . (:command "docker" :args ("run" "--rm" "-i" "mcp/everything")))
          ))
  )

(use-package agent-shell
  :commands agent-shell
  :bind (:map agent-shell-mode-map
         ("C-<tab>" . nil)
         ("C-f G" . agent-shell-help-menu)
         )
  :config
  (setq shell-maker-prompt-before-killing-buffer nil
        agent-shell-show-config-icons nil
        agent-shell-goose-authentication (agent-shell-make-goose-authentication :none t)
        agent-shell-goose-command (cons "goose-acp" (cdr agent-shell-goose-command))
        agent-shell-google-gemini-command (cons "gemini-acp" (cdr agent-shell-google-gemini-command))
        agent-shell-opencode-command (cons "opencode-acp" (cdr agent-shell-opencode-command))
        )
  (defun shell-maker-welcome-message (config) "")
)
