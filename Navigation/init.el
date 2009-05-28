
(require 'switch-file)

(eval-after-load "cc-mode"
  '(progn
     (define-key c-mode-map [(control meta o)] 'switch-file-major-mode)
     (define-key c++-mode-map [(control meta o)] 'switch-file-major-mode)))

(require 'isearch+)

;; make dired reuse the current buffer instead of opening new buffer
;; for each directory
(eval-after-load "dired"
  '(progn
     (defadvice dired-advertised-find-file (around dired-subst-directory activate)
       "Replace current buffer if file is a directory."
       (interactive)
       (let* ((orig (current-buffer))
              ;; (filename (dired-get-filename))
              (filename (dired-get-filename t t))
              (bye-p (file-directory-p filename)))
         ad-do-it
         (when (and bye-p (not (string-match "[/\\\\]\\.$" filename)))
           (kill-buffer orig))))
     (define-key dired-mode-map [(control x) (control q)] 'wdired-change-to-wdired-mode)
     (require 'dired-x)))

(eval-after-load "ido"
  '(progn
     (defvar ido-execute-command-cache nil)

     (defun ido-execute-command-refresh ()
       (interactive)
       (setq ido-execute-command-cache nil))
     
     (defun ido-execute-command ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-x "
          (progn
            (unless ido-execute-command-cache
              (mapatoms
               (lambda (s)
                 (when (commandp s)
                   (setq ido-execute-command-cache
                         (cons (format "%S" s) ido-execute-command-cache))))))
            ido-execute-command-cache)))))
     (global-set-key [(control q)] 'ido-execute-command)))

     