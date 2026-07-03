(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; do-eww-rename-buffer renames "*eww*" to "*<title> # eww*", so the default
;; markdown-live-preview-window-eww can't find it via get-buffer "*eww*".
(defun my-markdown-live-preview-window-eww (file)
  "Preview FILE with eww, compatible with custom eww buffer renaming."
  (eww-open-file file)
  (or (get-buffer "*eww*")
      (cl-loop for buf in (buffer-list)
               when (string-match-p "eww\\*\\'" (buffer-name buf))
               return buf)))

(eval-after-load "markdown-mode"
  '(setq markdown-live-preview-window-function
         #'my-markdown-live-preview-window-eww))
