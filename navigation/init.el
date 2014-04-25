
(autoload 'switch-file-major-mode "switch-file" nil t)

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map [(control meta o)] 'switch-file-major-mode)
     (define-key c++-mode-map [(control meta o)] 'switch-file-major-mode)))

(eval-after-load "ido"
  '(progn
     (global-set-key
      "\C-q"
      (lambda ()
        (interactive)
        (call-interactively
         (intern
          (ido-completing-read
           "M-x "
           (all-completions "" obarray 'commandp))))))
     ))

(defun rename-frame ()
  (interactive)
  (let ((new-name (read-from-minibuffer "Rename frame (to new name): ")))
    (setq frame-title-format new-name)))


(autoload 'elscreen-start "elscreen" nil t)
(eval-after-load "elscreen"
  '(progn
     (defun elscreen-get-screen-numbers-with-emphasis ()
       ""
       (interactive)
       (let ((elscreens (sort (elscreen-get-screen-list) '<))
             (emphased ""))

         (dolist (s elscreens)
           (setq emphased
                 (concat emphased (if (= (elscreen-get-current-screen) s)
                                      (propertize (number-to-string s)
                                                  ;;'face 'custom-variable-tag) " ")
                                                  ;;'face 'info-title-3)
                                                  'face 'font-lock-warning-face)
                                    ;;'face 'secondary-selection)
                                    (number-to-string s))
                         " ")))
         (message "screens: %s" emphased)))


     (defun elscreen-emph-prev ()
       (interactive)
       (elscreen-previous)
       (elscreen-get-screen-numbers-with-emphasis))

     (defun elscreen-emph-next ()
       (interactive)
       (elscreen-next)
       (elscreen-get-screen-numbers-with-emphasis))

     (setq elscreen-display-tab nil)
     (setq elscreen-prefix-key "\M-z")

     (global-set-key (kbd "M-[") 'elscreen-emph-prev)
     (global-set-key (kbd "M-]") 'elscreen-emph-next)
     ))

(global-set-key (kbd "M-[") (lambda () (interactive) (elscreen-start)))
(global-set-key (kbd "M-]") (lambda () (interactive) (elscreen-start)))


(autoload 'nav "nav" nil t)
(global-set-key "\M-`" 'nav)

(autoload 'minimap-create "minimap" nil t)
(autoload 'minimap-kill "minimap" nil t)
(setq minimap-flag nil)
(defun minimap-show ()
  (interactive)
  (if minimap-flag
      (progn
        (minimap-kill)
        (setq minimap-flag nil))
    (progn
      (minimap-create)
      (setq minimap-flag t))))
(global-set-key [(control \`)] 'minimap-show)


(autoload 'ffap-file-at-point "ffap" nil t)
(defun jump-to-file-at-point ()
  (interactive)
  (let ((file-path (ffap-file-at-point))
        (line-num 0))
    (if file-path
        (progn
          (save-excursion
            (search-forward-regexp "[^ ]:" (point-max) t)
            (if (looking-at "[0-9]+")
                (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
          (find-file-other-window file-path)
          (if (not (equal line-num 0))
              (goto-line line-num))))))

(defun app-open-file-at-point ()
  (interactive)
  (let ((file-path (ffap-file-at-point)))
    (if file-path
        (cond ((executable-find "gnome-open")
               (call-process-shell-command (format "gnome-open '%s'" file-path) nil 0))
              ((executable-find "xdg-open")
               (call-process-shell-command (format "xdg-open '%s'" file-path) nil 0)))
      (cond ((executable-find "nautilus")
             (call-process-shell-command (format "nautilus '%s'" (expand-file-name default-directory)) nil 0))
            ((executable-find "thunar")
             (call-process-shell-command (format "thunar '%s'" (expand-file-name default-directory)) nil 0)))
      )))

(define-key ctl-x-map "a" 'app-open-file-at-point)
(define-key ctl-x-map "f" 'jump-to-file-at-point)

(defun pycscope-index-files (top-directory)
  (interactive "DIndex files in directory: ")
  (call-process (in-emacs-d "navigation/pycscope.py") nil nil nil "-R"))


; ace-jump - quickly navigate to any character
(autoload 'ace-jump-char-mode "ace-jump-mode.el" nil t)
(setq ace-jump-mode-case-sensitive-search nil)
(global-set-key (kbd "C-x j") 'ace-jump-char-mode)
;   only use lowercase letters for lookup
(setq ace-jump-mode-move-keys
  (nconc (loop for i from ?a to ?z collect i)))

; find file in projects
(autoload 'find-file-in-project "find-file-in-project.el" nil t)
;; Function to create new functions that look for a specific pattern
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

(global-unset-key (kbd "C-f"))
(global-set-key (kbd "C-f c")
                (ffip-create-pattern-file-finder "*.c" "*.h" "*.cpp" "*.hpp" "*.cc"))
(global-set-key (kbd "C-f f")
                (ffip-create-pattern-file-finder "*.*"))
(global-set-key (kbd "C-f js")
                (ffip-create-pattern-file-finder "*.js"))
(global-set-key (kbd "C-f p")
                (ffip-create-pattern-file-finder "*.py"))
(global-set-key (kbd "C-f e")
                (ffip-create-pattern-file-finder "*.el"))

;; ag
(autoload 'ag "ag.el" nil t)
(autoload 'ag-dired "ag.el" nil t)
(autoload 'ag/dwim-at-point "ag.el" nil t)

(defun ag-here (string)
  (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))))
  (ag string default-directory))

(defun ag-dired-here (pattern)
  (interactive "sFile pattern: ")
  (ag-dired default-directory pattern))

(global-set-key (kbd "C-f s") 'ag-here)
(global-set-key (kbd "C-f d") 'ag-dired-here)
