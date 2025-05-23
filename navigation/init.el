(eval-after-load "ido"
  '(progn
     (defun ido-occasional-completing-read
         (prompt collection
                 &optional predicate require-match initial-input
                 hist def inherit-input-method)
       "Use `ido-completing-read' if the collection isn't too large.
Fall back to `completing-read' otherwise."
       (let ((filtered-collection
              (all-completions "" collection predicate)))
         (if (<= (length filtered-collection) 30000)
             (ido-completing-read
              prompt filtered-collection nil
              require-match initial-input hist
              def nil)
           (completing-read
            prompt collection predicate
            require-match initial-input hist
            def inherit-input-method))))

     (defmacro with-ido-completion (fun)
       "Wrap FUN in another interactive function with ido completion."
       `(defun ,(intern (concat (symbol-name fun) "/with-ido")) ()
          ,(format "Forward to `%S' with ido completion." fun)
          (interactive)
          (let ((completing-read-function
                 'ido-occasional-completing-read))
            (call-interactively #',fun))))

     (global-set-key
      "\C-q"
      (with-ido-completion execute-extended-command))
     ))

(defun rename-frame ()
  (interactive)
  (let ((new-name (read-from-minibuffer "Rename frame (to new name): ")))
    (setq frame-title-format new-name)))

(use-package perspective
  :custom
  (persp-mode-prefix-key (kbd "M-z"))
  :config
  (setq persp-initial-frame-name "0"
        persp-modestring-short t
        )

  (defun persp-emph ()
    (interactive)
    (let* ((names (persp-names))
           (msg (string-join (mapcar (lambda (s)
                                       (if (string= s (persp-current-name))
                                           (propertize s 'face 'font-lock-warning-face)
                                         s))
                                     names)
                             " ")))
      (message msg)
      ))

  (defun persp-emph-prev ()
    (interactive)
    (persp-prev)
    (persp-emph))

  (defun persp-emph-next ()
    (interactive)
    (persp-next)
    (persp-emph))


  (persp-mode)
  :bind (("M-[" . persp-emph-prev)
         ("M-]" . persp-emph-next)
         )

  )

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
  (let* ((file-path (ffap-file-at-point))
         (open-exec (or (executable-find "xdg-open")
                        (executable-find "gnome-open")
                        (executable-find "nautilus")
                        (executable-find "thunar")
                        ))
         (url-path (ffap-url-at-point)))
    (cond
     (url-path (browse-url url-path))
     (file-path (call-process-shell-command
                 (format "%s '%s'" open-exec (expand-file-name file-path))
                 nil
                 0))
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
      (nconc (cl-loop for i from ?a to ?z collect i)))

                                        ; find file in projects
(autoload 'find-file-in-project "find-file-in-project.el" nil t)
;; Function to create new functions that look for a specific pattern
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

(defun project-try-local (dir)
  (let ((root (locate-dominating-file dir ".project")))
    (and root (cons 'transient root))))

(add-hook 'project-find-functions #'project-try-local)

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
(autoload 'ag-dired-regexp "ag.el" nil t)
(autoload 'ag/dwim-at-point "ag.el" nil t)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)
(eval-after-load "ag"
  '(progn
     (setq ag-reuse-buffers t
           ag-group-matches nil
           ag-executable "rg"
           ag-arg-literal "--fixed-strings"
           ag-arg-group '("--heading" "--no-heading")
           ag-arg-extra '("--line-number" "--column")
           ag-arguments '("--smart-case")
           ag-arg-filesearch "--color never --smart-case --files -g"
           )
     (define-key ag-mode-map (kbd "q")
                 (lambda () (interactive)
                   (let (kill-buffer-query-functions) (kill-buffer))))))

(defun ag-here (string)
  (interactive (list (read-from-minibuffer "Search string: " (ag/dwim-at-point))))
  (ag string default-directory))

(defun ag-dired-here (pattern)
  (interactive "sFile pattern: ")
  (ag-dired-regexp default-directory (concat "*" pattern "*")))

(global-set-key (kbd "C-f s") 'ag-here)
(global-set-key (kbd "C-f d") 'ag-dired-here)

;; env

(autoload 'er/expand-region "expand-region.el" nil t)
(global-set-key (kbd "M-2") #'er/expand-region)

(use-package dumb-jump
  :custom
  (dumb-jump-disable-obsolete-warnings t)
  (dumb-jump-force-searcher 'rg)
  :bind (("C-]" . dumb-jump-go)
         ("C-}" . dumb-jump-back)
         )
  )

(use-package engine-mode
  :config
  (defengine github
             "https://github.com/search?q=%s"
             :browser 'browse-url-chromium)
  :bind (("C-f v g" . engine/search-github)
         )
  )

(use-package hideshow-org
  :commands (hs-org/minor-mode)
  :hook
  ((prog-mode . hs-org/minor-mode))
  )

(use-package inhibit-mouse
  :config
  (inhibit-mouse-mode)
  )
