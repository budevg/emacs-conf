
(define-key lisp-mode-shared-map  "\C-m" 'newline-and-indent)

(setq inferior-lisp-program "clisp")

;;(autoload 'slime "slime" nil t) 
;;(autoload 'slime-mode "slime" nil t)
;;(autoload 'slime-lisp-mode-hook "slime")

;;(add-hook 'lisp-mode-hook 'slime-lisp-mode-hook)

(autoload 'inferior-lisp-mode "inf-lisp" nil t)
(defun lisp-execute-buffer ()
  (interactive)
  (lisp-eval-region (point-min) (point-max)))

(defun run-lisp-new (cmd)
  "Run an inferior Lisp process, input and output via buffer `*inferior-lisp*'.
If there is a process already running in `*inferior-lisp*', just switch
to that buffer.
With argument, allows you to edit the command line (default is value
of `inferior-lisp-program').  Runs the hooks from
`inferior-lisp-mode-hook' (after the `comint-mode-hook' is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"
  (interactive (list (if current-prefix-arg
			 (read-string "Run lisp: " inferior-lisp-program)
		       inferior-lisp-program)))
  (if (not (comint-check-proc "*inferior-lisp*"))
      (let ((cmdlist (split-string cmd)))
	(set-buffer (apply (function make-comint)
			   "inferior-lisp" (car cmdlist) nil (cdr cmdlist)))
	(inferior-lisp-mode)))
  (setq inferior-lisp-buffer "*inferior-lisp*")
  (switch-to-buffer-other-window "*inferior-lisp*"))

(eval-after-load "lisp-mode"
  '(progn
     (define-key lisp-mode-map [(control shift p)] 'run-lisp-new)))
(eval-after-load "inf-lisp"
  '(progn
     (define-key lisp-mode-map [(control c) (control c)] 'lisp-execute-buffer)))
