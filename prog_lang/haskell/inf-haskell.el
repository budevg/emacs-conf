;;; inf-haskell.el --- Interaction with an inferior Haskell process.

;; Copyright (C) 2004, 2005, 2006, 2007, 2008, 2009  Free Software Foundation, Inc.

;; Author: Stefan Monnier <monnier@iro.umontreal.ca>
;; Keywords: Haskell

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; The code is made of 2 parts: a major mode for the buffer that holds the
;; inferior process's session and a minor mode for use in source buffers.

;; Todo:

;; - Check out Shim for ideas.
;; - i-h-load-buffer and i-h-send-region.

;;; Code:

(require 'comint)
(require 'shell)			;For directory tracking.
(require 'compile)
(require 'haskell-mode)
(eval-when-compile (require 'cl))

;; XEmacs compatibility.

(unless (fboundp 'subst-char-in-string)
  (defun subst-char-in-string (fromchar tochar string &optional inplace)
    ;; This is Haskell-mode, we don't want no stinkin' `aset'.
    (apply 'string (mapcar (lambda (c) (if (eq c fromchar) tochar c)) string))))

(unless (fboundp 'make-temp-file)
  (defun make-temp-file (prefix &optional dir-flag)
    (catch 'done
      (while t
        (let ((f (make-temp-name (expand-file-name prefix (temp-directory)))))
          (condition-case ()
              (progn
                (if dir-flag (make-directory f)
                  (write-region "" nil f nil 'silent nil))
                (throw 'done f))
            (file-already-exists t)))))))

(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp rep string)
    (replace-in-string string regexp rep)))

;; Here I depart from the inferior-haskell- prefix.
;; Not sure if it's a good idea.
(defcustom haskell-program-name
  ;; Arbitrarily give preference to hugs over ghci.
  (or (cond
       ((not (fboundp 'executable-find)) nil)
       ((executable-find "hugs") "hugs \"+.\"")
       ((executable-find "ghci") "ghci"))
      "hugs \"+.\"")
  "The name of the command to start the inferior Haskell process.
The command can include arguments."
  ;; Custom only supports the :options keyword for a few types, e.g. not
  ;; for string.
  ;; :options '("hugs \"+.\"" "ghci")
  :group 'haskell
  :type '(choice string (repeat string)))

(defconst inferior-haskell-info-xref-re
  "\t-- Defined at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?$")

(defconst inferior-haskell-module-re
  "\t-- Defined in \\(.+\\)$"
  "Regular expression for matching module names in :info.")

(defvar inferior-haskell-multiline-prompt-re
  "^\\*?[[:upper:]][\\._[:alnum:]]*\\(?: \\*?[[:upper:]][\\._[:alnum:]]*\\)*| "
  "Regular expression for matching multiline prompt (the one inside :{ ... :} blocks).")

(defconst inferior-haskell-error-regexp-alist
  ;; The format of error messages used by Hugs.
  `(("^ERROR \"\\(.+?\\)\"\\(:\\| line \\)\\([0-9]+\\) - " 1 3)
    ;; Format of error messages used by GHCi.
    ("^\\(.+?\\):\\([0-9]+\\):\\(\\([0-9]+\\):\\)?\\( \\|\n *\\)\\(Warning\\)?"
     1 2 4 ,@(if (fboundp 'compilation-fake-loc)
                 '((6) nil (5 '(face nil font-lock-multiline t)))))
    ;; Runtime exceptions, from ghci.
    ("^\\*\\*\\* Exception: \\(.+?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\)): .*"
     1 ,@(if (fboundp 'compilation-fake-loc) '((2 . 4) (3 . 5)) '(2 3)))
    ;; GHCi uses two different forms for line/col ranges, depending on
    ;; whether it's all on the same line or not :-( In Emacs-23, I could use
    ;; explicitly numbered subgroups to merge the two patterns.
    ("^\\*\\*\\* Exception: \\(.+?\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\): .*"
     1 2 ,(if (fboundp 'compilation-fake-loc) '(3 . 4) 3))
    ;; Info messages.  Not errors per se.
    ,@(when (fboundp 'compilation-fake-loc)
        `(;; Other GHCi patterns used in type errors.
          ("^[ \t]+at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)$"
           1 2 (3 . 4) 0)
          ;; Foo.hs:318:80:
          ;;     Ambiguous occurrence `Bar'
          ;;     It could refer to either `Bar', defined at Zork.hs:311:5
          ;; 		          or `Bar', imported from Bars at Frob.hs:32:0-16
          ;; 				       (defined at Location.hs:97:5)
          ("[ (]defined at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\))?$" 1 2 3 0)
          ("imported from .* at \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)-\\([0-9]+\\)$"
           1 2 (3 . 4) 0)
          ;; Info xrefs.
          (,inferior-haskell-info-xref-re 1 2 (3 . 4) 0))))
  "Regexps for error messages generated by inferior Haskell processes.
The format should be the same as for `compilation-error-regexp-alist'.")

(defcustom inferior-haskell-find-project-root t
  "If non-nil, try and find the project root directory of this file.
This will either look for a Cabal file or a \"module\" statement in the file."
  :group 'haskell
  :type 'boolean)

(define-derived-mode inferior-haskell-mode comint-mode "Inf-Haskell"
  "Major mode for interacting with an inferior Haskell process."
  (set (make-local-variable 'comint-prompt-regexp)
       ;; Whay the backslash in [\\._[:alnum:]]?
       "^\\*?[[:upper:]][\\._[:alnum:]]*\\(?: \\*?[[:upper:]][\\._[:alnum:]]*\\)*\\( λ\\)?> \\|^λ?> $\\|^λ: $")
  (set (make-local-variable 'comint-input-autoexpand) nil)
  (add-hook 'comint-preoutput-filter-functions
            'inferior-haskell-send-decl-post-filter)
  (add-hook 'comint-output-filter-functions 'inferior-haskell-spot-prompt nil t)

  ;; Setup directory tracking.
  (set (make-local-variable 'shell-cd-regexp) ":cd")
  (condition-case nil
      (shell-dirtrack-mode 1)
    (error      ;The minor mode function may not exist or not accept an arg.
     (set (make-local-variable 'shell-dirtrackp) t)
     (add-hook 'comint-input-filter-functions 'shell-directory-tracker
               nil 'local)))

  ;; Setup `compile' support so you can just use C-x ` and friends.
  (set (make-local-variable 'compilation-error-regexp-alist)
       inferior-haskell-error-regexp-alist)
  (set (make-local-variable 'compilation-first-column) 0) ;GHCI counts from 0.
  (if (and (not (boundp 'minor-mode-overriding-map-alist))
           (fboundp 'compilation-shell-minor-mode))
      ;; If we can't remove compilation-minor-mode bindings, at least try to
      ;; use compilation-shell-minor-mode, so there are fewer
      ;; annoying bindings.
      (compilation-shell-minor-mode 1)
    ;; Else just use compilation-minor-mode but without its bindings because
    ;; things like mouse-2 are simply too annoying.
    (compilation-minor-mode 1)
    (let ((map (make-sparse-keymap)))
      (dolist (keys '([menu-bar] [follow-link]))
        ;; Preserve some of the bindings.
        (define-key map keys (lookup-key compilation-minor-mode-map keys)))
      (add-to-list 'minor-mode-overriding-map-alist
                   (cons 'compilation-minor-mode map)))))

(defun inferior-haskell-string-to-strings (string)
  "Split the STRING into a list of strings."
  (let ((i (string-match "[\"]" string)))
    (if (null i) (split-string string)	; no quoting:  easy
      (append (unless (eq i 0) (split-string (substring string 0 i)))
	      (let ((rfs (read-from-string string i)))
		(cons (car rfs)
		      (inferior-haskell-string-to-strings
		       (substring string (cdr rfs)))))))))

(defun inferior-haskell-command (arg)
  (inferior-haskell-string-to-strings
   (if (null arg) haskell-program-name
     (read-string "Command to run haskell: " haskell-program-name))))

(defvar inferior-haskell-buffer nil
  "The buffer in which the inferior process is running.")

(defun inferior-haskell-start-process (command)
  "Start an inferior haskell process.
With universal prefix \\[universal-argument], prompts for a COMMAND,
otherwise uses `haskell-program-name'.
It runs the hook `inferior-haskell-hook' after starting the process and
setting up the inferior-haskell buffer."
  (interactive (list (inferior-haskell-command current-prefix-arg)))
  (setq inferior-haskell-buffer
	(apply 'make-comint "haskell" (car command) nil (cdr command)))
  (with-current-buffer inferior-haskell-buffer
    (inferior-haskell-mode)
    (run-hooks 'inferior-haskell-hook)))

(defun inferior-haskell-process (&optional arg)
  (or (if (buffer-live-p inferior-haskell-buffer)
	  (get-buffer-process inferior-haskell-buffer))
      (progn
	(let ((current-prefix-arg arg))
	  (call-interactively 'inferior-haskell-start-process))
	;; Try again.
	(inferior-haskell-process arg))))

;;;###autoload
(defalias 'run-haskell 'switch-to-haskell)
;;;###autoload
(defun switch-to-haskell (&optional arg)
  "Show the inferior-haskell buffer.  Start the process if needed."
  (interactive "P")
  (let ((proc (inferior-haskell-process arg)))
    (pop-to-buffer (process-buffer proc))))

(eval-when-compile
  (unless (fboundp 'with-selected-window)
    (defmacro with-selected-window (win &rest body)
      `(save-selected-window
         (select-window ,win)
         ,@body))))

(defcustom inferior-haskell-wait-and-jump nil
  "If non-nil, wait for file loading to terminate and jump to the error."
  :type 'boolean
  :group 'haskell)

(defvar inferior-haskell-send-decl-post-filter-on nil)
(make-variable-buffer-local 'inferior-haskell-send-decl-post-filter-on)

(defun inferior-haskell-send-decl-post-filter (string)
  (when (and inferior-haskell-send-decl-post-filter-on
             #1=(string-match inferior-haskell-multiline-prompt-re string))
    ;; deleting sequence of `%s|' multiline promts
    (while #1#
      (setq string (substring string (match-end 0))))    
    ;; deleting regular prompts
    (setq string (replace-regexp-in-string comint-prompt-regexp "" string)
          ;; turning off this post-filter
          inferior-haskell-send-decl-post-filter-on nil))  
  string)

(defvar inferior-haskell-seen-prompt nil)
(make-variable-buffer-local 'inferior-haskell-seen-prompt)

(defun inferior-haskell-spot-prompt (string)
  (let ((proc (get-buffer-process (current-buffer))))
    (when proc
      (save-excursion
        (goto-char (process-mark proc))
        (if (re-search-backward comint-prompt-regexp
                                (line-beginning-position) t)
            (setq inferior-haskell-seen-prompt t))))))

(defun inferior-haskell-wait-for-prompt (proc &optional timeout)
  "Wait until PROC sends us a prompt.
The process PROC should be associated to a comint buffer."
  (with-current-buffer (process-buffer proc)
    (while (progn
             (goto-char comint-last-input-end)
             (not (or inferior-haskell-seen-prompt
                      (setq inferior-haskell-seen-prompt
                            (re-search-forward comint-prompt-regexp nil t))
                      (not (accept-process-output proc timeout))))))
    (unless inferior-haskell-seen-prompt
      (error "Can't find the prompt"))))

(defvar inferior-haskell-cabal-buffer nil)

(defun inferior-haskell-cabal-of-buf (buf)
  (require 'haskell-cabal)
  (with-current-buffer buf
    (or (and (buffer-live-p inferior-haskell-cabal-buffer)
             inferior-haskell-cabal-buffer)
        (and (not (local-variable-p 'inferior-haskell-cabal-buffer
                                    ;; XEmacs needs this argument.
                                    (current-buffer)))
             (set (make-local-variable 'inferior-haskell-cabal-buffer)
                  (haskell-cabal-find-file))))))

(defun inferior-haskell-find-project-root (buf)
  (with-current-buffer buf
    (let* (
           (cabal-file (inferior-haskell-cabal-of-buf buf))
           (cabal (when cabal-file
                    (find-file-noselect cabal-file)))
           )
      (or (when cabal
            (with-current-buffer cabal
              (let ((hsd (haskell-cabal-get-setting "hs-source-dirs")))
                (if (null hsd)
                    ;; If there's a Cabal file with no Hs-Source-Dirs, then
                    ;; just use the Cabal file's directory.
                    default-directory
                  ;; If there is an HSD, then check that it's an existing
                  ;; dir (otherwise, it may be a list of dirs and we don't
                  ;; know what to do with those).  If it doesn't exist, then
                  ;; give up.
                  (if (file-directory-p hsd) (expand-file-name hsd))))))
          ;; If there's no Cabal file or it's not helpful, try to look for
          ;; a "module" statement and count the number of "." in the
          ;; module name.
          (save-excursion
            (goto-char (point-min))
            (let ((case-fold-search nil))
              (when (re-search-forward
                     "^module[ \t]+\\(\\(?:\\sw\\|[.]\\)+\\)" nil t)
                (let* ((dir default-directory)
                       (module (match-string 1))
                       (pos 0))
                  (while (string-match "\\." module pos)
                    (setq pos (match-end 0))
                    (setq dir (expand-file-name ".." dir)))
                  ;; Let's check that the module name matches the file name,
                  ;; otherwise the project root is probably not what we think.
                  (if (eq t (compare-strings
                             (file-name-sans-extension buffer-file-name)
                             nil nil
                             (expand-file-name
                              (replace-regexp-in-string "\\." "/" module)
                              dir)
                             nil nil t))
                      dir
                    ;; If they're not equal, it means the local directory
                    ;; hierarchy doesn't match the module name.  This seems
                    ;; odd, so let's warn the user about it.  May help us
                    ;; debug this code as well.
                    (message "Ignoring inconsistent `module' info: %s in %s"
                             module buffer-file-name)
                    nil)))))))))



;;;###autoload
(defun inferior-haskell-load-file (&optional reload)
  "Pass the current buffer's file to the inferior haskell process.
If prefix arg \\[universal-argument] is given, just reload the previous file."
  (interactive "P")
  ;; Save first, so we're sure that `buffer-file-name' is non-nil afterward.
  (save-buffer)
  (let ((buf (current-buffer))
        (file buffer-file-name)
	(proc (inferior-haskell-process)))
    (if file
        (with-current-buffer (process-buffer proc)
          (compilation-forget-errors)
          (let ((parsing-end (marker-position (process-mark proc)))
                root)
            ;; Go to the root of the Cabal project, if applicable.
            (when (and inferior-haskell-find-project-root
                       (setq root (inferior-haskell-find-project-root buf)))
              ;; Not sure if it's useful/needed and if it actually works.
              (unless (equal default-directory root)
                (setq default-directory root)
                (inferior-haskell-send-command
                 proc (concat ":cd " default-directory)))
              (setq file (file-relative-name file)))
            (inferior-haskell-send-command
             proc (if reload ":reload"
                    (concat ":load \""
                            ;; Espace the backslashes that may occur in file names.
                            (replace-regexp-in-string "[\\\"]" "\\\\\&" file)
                            "\"")))
            ;; Move the parsing-end marker *after* sending the command so
            ;; that it doesn't point just to the insertion point.
            ;; Otherwise insertion may move the marker (if done with
            ;; insert-before-markers) and we'd then miss some errors.
            (if (boundp 'compilation-parsing-end)
                (if (markerp compilation-parsing-end)
                    (set-marker compilation-parsing-end parsing-end)
                  (setq compilation-parsing-end parsing-end))))
          (with-selected-window (display-buffer (current-buffer) nil 'visible)
            (end-of-buffer))
          ;; Use compilation-auto-jump-to-first-error if available.
          ;; (if (and (boundp 'compilation-auto-jump-to-first-error)
          ;;          compilation-auto-jump-to-first-error
          ;;          (boundp 'compilation-auto-jump-to-next))
          ;;     (setq compilation-auto-jump-to-next t)
          (when inferior-haskell-wait-and-jump
            (inferior-haskell-wait-for-prompt proc)
            (ignore-errors                  ;Don't beep if there were no errors.
              (next-error))))
      (error "No file associated with buffer"))))

(defvar inferior-haskell-run-command ":main")

;;;###autoload
(defun inferior-haskell-load-and-run (command)
  "Pass the current buffer's file to haskell and then run a COMMAND."
  (interactive
   (list
    (if (and inferior-haskell-run-command (not current-prefix-arg))
        inferior-haskell-run-command
      (read-string "Command to run: " nil nil inferior-haskell-run-command))))
  (setq inferior-haskell-run-command command)
  (let* ((inferior-haskell-errors nil)
         (neh (lambda () (setq inferior-haskell-errors t))))
    (unwind-protect
        (let ((inferior-haskell-wait-and-jump t))
          (add-hook 'next-error-hook neh)
          (inferior-haskell-load-file))
      (remove-hook 'next-error-hook neh))
    (unless inferior-haskell-errors
      (inferior-haskell-send-command (inferior-haskell-process) command)
      (switch-to-haskell))))

(defun inferior-haskell-send-command (proc str)
  (setq str (concat str "\n"))
  (with-current-buffer (process-buffer proc)
    (inferior-haskell-wait-for-prompt proc)
    (goto-char (process-mark proc))
    (insert-before-markers str)
    (move-marker comint-last-input-end (point))
    (setq inferior-haskell-seen-prompt nil)
    (comint-send-string proc str)))

(defun inferior-haskell-reload-file ()
  "Tell the inferior haskell process to reread the current buffer's file."
  (interactive)
  (inferior-haskell-load-file 'reload))

(defun inferior-haskell-wrap-decl (code)
  "Wrap declaration code into :{ ... :}."
  (setq code (concat code "\n"))
  (concat ":{\n"
          (if (string-match (concat "^\\s-*"
                                    haskell-ds-start-keywords-re)
                            code)
              ;; non-fun-decl
              code
            ;; fun-decl, wrapping into let { .. (; ..)* }
            (concat "let {\n"
                    (mapconcat
                     ;; adding 2 whitespaces to each line
                     (lambda (decl)
                       (mapconcat (lambda (s)
                                    (concat "  " s))
                                  (split-string decl "\n")
                                  "\n"))
                     ;; splitting function case-decls
                     (let (decls)
                       (while (string-match "^\\(\\w+\\).*\n*\\(?:\\s-+.*\n+\\)*" code)
                         (push (match-string 0 code) decls)
                         (setq code (substring code (match-end 0))))
                       (reverse decls))
                     "\n;\n")
                    "\n}"))
          "\n:}\n"))

(defun inferior-haskell-flash-decl (start end &optional timeout)
  "Temporarily highlight declaration."
  (let ((overlay (make-overlay start end)))
    (overlay-put overlay 'face 'secondary-selection)
    (run-with-timer (or timeout 0.2) nil 'delete-overlay overlay)))

;;;###autoload
(defun inferior-haskell-send-decl ()
  "Send current declaration to inferior-haskell process."
  (interactive)
  (require 'haskell-decl-scan)
  (save-excursion
    (goto-char (1+ (point)))
    (let* ((proc (inferior-haskell-process))
           (start (or (haskell-ds-backward-decl) (point-min)))
           (end (or (haskell-ds-forward-decl) (point-max)))
           (raw-decl (buffer-substring start end)))
      ;; enter multiline-prompt-cutting-mode
      (with-current-buffer (process-buffer proc)
        (setq inferior-haskell-send-decl-post-filter-on t))
      ;; flash decl
      (inferior-haskell-flash-decl start end)
      ;; send decl
      (comint-send-string proc (inferior-haskell-wrap-decl raw-decl))
      ;; send preview
      (inferior-haskell-send-command
       proc
       (let* ((str (remove ?\n raw-decl))
              (len (min 15 (length str))))
         (concat "-- evaluating {: "
                 (substring str 0 len)
                 (if (= 15 len) ".." "")
                 " :}"))))))

(defun inferior-haskell-get-result (inf-expr)
  "Submit the expression `inf-expr' to ghci and read the result."
  (let ((proc (inferior-haskell-process)))
    (with-current-buffer (process-buffer proc)
      (let ((parsing-end                ; Remember previous spot.
             (marker-position (process-mark proc))))
        (inferior-haskell-send-command proc inf-expr)
        ;; Find new point.
        (inferior-haskell-wait-for-prompt proc)
        (goto-char (point-max))
        ;; Back up to the previous end-of-line.
        (end-of-line 0)
        ;; Extract the output
        (buffer-substring-no-properties
         (save-excursion (goto-char parsing-end)
                         (line-beginning-position 2))
         (point))))))

;;;###autoload
(defun inferior-haskell-type (expr &optional insert-value)
  "Query the haskell process for the type of the given expression.
If optional argument `insert-value' is non-nil, insert the type above point
in the buffer.  This can be done interactively with the \\[universal-argument] prefix.
The returned info is cached for reuse by `haskell-doc-mode'."
  (interactive
   (let ((sym (haskell-ident-at-point)))
     (list (read-string (if (> (length sym) 0)
                            (format "Show type of (default %s): " sym)
                          "Show type of: ")
                        nil nil sym)
           current-prefix-arg)))
  (if (string-match "\\`\\s_+\\'" expr) (setq expr (concat "(" expr ")")))
  (let ((type (inferior-haskell-get-result (concat ":type " expr))))
    (if (not (string-match (concat "^\\(" (regexp-quote expr)
                                   "[ \t\n]+::[ \t\n]*\\(.\\|\n\\)*\\)")
                           type))
        (error "No type info: %s" type)
      (progn
        (setf type (match-string 1 type))
        ;; Cache for reuse by haskell-doc.
        (when (and (boundp 'haskell-doc-mode) haskell-doc-mode
                   (boundp 'haskell-doc-user-defined-ids)
                   ;; Haskell-doc only works for idents, not arbitrary expr.
                   (string-match "\\`(?\\(\\s_+\\|\\(\\sw\\|\\s'\\)+\\)?[ \t]*::[ \t]*"
                                 type))
          (let ((sym (match-string 1 type)))
            (setq haskell-doc-user-defined-ids
                  (cons (cons sym (substring type (match-end 0)))
                        (delq (assoc sym haskell-doc-user-defined-ids)
                              haskell-doc-user-defined-ids)))))

        (if (interactive-p) (message "%s" type))
        (when insert-value
          (beginning-of-line)
          (insert type "\n"))
        type))))

;;;###autoload
(defun inferior-haskell-kind (type)
  "Query the haskell process for the kind of the given expression."
  (interactive
   (let ((type (haskell-ident-at-point)))
     (list (read-string (if (> (length type) 0)
                            (format "Show kind of (default %s): " type)
                          "Show kind of: ")
                        nil nil type))))
  (let ((result (inferior-haskell-get-result (concat ":kind " type))))
    (if (interactive-p) (message "%s" result))
    result))

;;;###autoload
(defun inferior-haskell-info (sym)
  "Query the haskell process for the info of the given expression."
  (interactive
   (let ((sym (haskell-ident-at-point)))
     (list (read-string (if (> (length sym) 0)
                            (format "Show info of (default %s): " sym)
                          "Show info of: ")
                        nil nil sym))))
  (let ((result (inferior-haskell-get-result (concat ":info " sym))))
    (if (interactive-p) (message "%s" result))
    result))

;;;###autoload
(defun inferior-haskell-find-definition (sym)
  "Attempt to locate and jump to the definition of the given expression."
  (interactive
   (let ((sym (haskell-ident-at-point)))
     (list (read-string (if (> (length sym) 0)
                            (format "Find definition of (default %s): " sym)
                          "Find definition of: ")
                        nil nil sym))))
  (let ((info (inferior-haskell-info sym)))
    (if (not (string-match inferior-haskell-info-xref-re info))
        (error "No source information available")
      (let ((file (match-string-no-properties 1 info))
            (line (string-to-number
                   (match-string-no-properties 2 info)))
            (col (string-to-number
                  (match-string-no-properties 3 info))))
        (when file
          (with-current-buffer (process-buffer (inferior-haskell-process))
            ;; The file name is relative to the process's cwd.
            (setq file (expand-file-name file)))
          ;; Push current location marker on the ring used by `find-tag'
          (require 'etags)
          (ring-insert find-tag-marker-ring (point-marker))
          (pop-to-buffer (find-file-noselect file))
          (when line
            (goto-line line)
            (when col (move-to-column col))))))))

;;; Functions to find the documentation of a given function.
;;
;; TODO for this section:
;;
;; * Support fetching of local Haddock docs pulled directly from source files.
;; * Display docs locally? w3m?

(defcustom inferior-haskell-use-web-docs
  'fallback
  "Whether to use the online documentation.  Possible values:
`never', meaning always use local documentation, unless the local
file doesn't exist, when do nothing, `fallback', which means only
use the online documentation when the local file doesn't exist,
or `always', meaning always use the online documentation,
regardless of existance of local files.  Default is `fallback'."
  :group 'haskell
  :type '(choice (const :tag "Never" never)
                 (const :tag "As fallback" fallback)
                 (const :tag "Always" always)))

(defcustom inferior-haskell-web-docs-base
  "http://haskell.org/ghc/docs/latest/html/libraries/"
  "The base URL of the online libraries documentation.
This will only be used if the value of `inferior-haskell-use-web-docs'
is `always' or `fallback'."
  :group 'haskell
  :type 'string)

(defcustom haskell-package-manager-name "ghc-pkg"
  "Name of the program to consult regarding package details."
  :group 'haskell
  :type 'string)

(defcustom haskell-package-conf-file
  (condition-case nil
      (with-temp-buffer
        (call-process "ghc" nil t nil "--print-libdir")
        (expand-file-name "package.conf"
                          (buffer-substring (point-min) (1- (point-max)))))
    ;; Don't use `ignore-errors' because this form is not byte-compiled :-(
    (error nil))
  "Where the package configuration file for the package manager resides.
By default this is set to `ghc --print-libdir`/package.conf."
  :group 'haskell
  :type 'string)

(defun inferior-haskell-get-module (sym)
  "Fetch the module in which SYM is defined."
  (let ((info (inferior-haskell-info sym)))
    (unless (string-match inferior-haskell-module-re info)
      (error
       "No documentation information available.  Did you forget to C-c C-l?"))
    (let ((module-name (match-string-no-properties 1 info)))
      ;; Handles GHC 7.4.1+ which quotes module names like
      ;; `System.Random', whereas previous GHC did not quote at all.
      (if (string= "`" (substring module-name 0 1))
          (substring module-name 1 (- (length module-name) 1))
        module-name))))

(defun inferior-haskell-query-ghc-pkg (&rest args)
  "Send ARGS to `haskell-package-manager-name'.
Insert the output into the current buffer."
  (apply 'call-process haskell-package-manager-name nil t nil args))

(defun inferior-haskell-get-package-list ()
  "Get the list of packages from `haskell-package-manager-name'."
  (with-temp-buffer
    (inferior-haskell-query-ghc-pkg "--simple-output" "list")
    (split-string (buffer-substring (point-min) (point-max)))))

(defun inferior-haskell-compute-module-alist ()
  "Compute a list mapping modules to package names and haddock URLs using ghc-pkg."
  (message "Generating module alist...")
  (let ((module-alist ()))
    (with-temp-buffer
      (dolist (package (inferior-haskell-get-package-list))
        (erase-buffer)
        (inferior-haskell-query-ghc-pkg "describe" package)

        (let ((package-w/o-version
               (replace-regexp-in-string "[-.0-9]*\\'" "" package))
              ;; Find the Haddock documentation URL for this package
              (haddock
               (progn
                 (goto-char (point-min))
                 (when (re-search-forward "haddock-html:[ \t]+\\(.*[^ \t\n]\\)"
                                          nil t)
                   (match-string 1)))))

          ;; Fetch the list of exposed modules for this package
          (goto-char (point-min))
          (when (re-search-forward "^exposed-modules:\\(.*\\(\n[ \t].*\\)*\\)"
                                   nil t)
            (dolist (module (split-string (match-string 1)))
              (push (list module package-w/o-version haddock)
                    module-alist)))))

      (message "Generating module alist... done")
      module-alist)))


(defcustom inferior-haskell-module-alist-file
  ;; (expand-file-name "~/.inf-haskell-module-alist")
  (expand-file-name (concat "inf-haskell-module-alist-"
                            (number-to-string (user-uid)))
                    (if (fboundp 'temp-directory)
                        (temp-directory)
                      temporary-file-directory))
  "Where to save the module -> package lookup table.
Set this to nil to never cache to a file."
  :group 'haskell
  :type '(choice (const :tag "Don't cache to file" nil) string))

(defvar inferior-haskell-module-alist nil
  "Association list of modules to their packages.
Each element is of the form (MODULE PACKAGE HADDOCK), where
MODULE is the name of a module,
PACKAGE is the package it belongs to, and
HADDOCK is the path to that package's Haddock documentation.

This is calculated on-demand using `inferior-haskell-compute-module-alist'.
It's also cached in the file `inferior-haskell-module-alist-file',
so that it can be obtained more quickly next time.")

(defun inferior-haskell-module-alist ()
  "Get the module alist from cache or ghc-pkg's info."
  (or
   ;; If we already have computed the alist, use it...
   inferior-haskell-module-alist
   (setq inferior-haskell-module-alist
         (or
          ;; ...otherwise try to read it from the cache file...
          (and
           inferior-haskell-module-alist-file
           (file-readable-p inferior-haskell-module-alist-file)
           (file-newer-than-file-p inferior-haskell-module-alist-file
                                   haskell-package-conf-file)
           (with-temp-buffer
             (insert-file-contents inferior-haskell-module-alist-file)
             (goto-char (point-min))
             (prog1 (read (current-buffer))
               (message "Read module alist from file cache."))))

          ;; ...or generate it again and save it in a file for later.
          (let ((alist (inferior-haskell-compute-module-alist)))
            (when inferior-haskell-module-alist-file
              (with-temp-buffer
                (print alist (current-buffer))
                ;; Do the write to a temp file first, then rename it.
                ;; This makes it more atomic, and suffers from fewer security
                ;; holes related to race conditions if the file is in /tmp.
                (let ((tmp (make-temp-file inferior-haskell-module-alist-file)))
                  (write-region (point-min) (point-max) tmp)
                  (rename-file tmp inferior-haskell-module-alist-file
                               'ok-if-already-exists))))
            alist)))))

(defvar inferior-haskell-ghc-internal-ident-alist
  ;; FIXME: Fill this table, ideally semi-automatically.
  '(("GHC.Base.return" . "Control.Monad.return")
    ("GHC.List" . "Data.List")))

(defun inferior-haskell-map-internal-ghc-ident (ident)
  "Try to translate some internal GHC identifier to its alter ego in haskell docs."
  (let ((head ident)
        (tail "")
        remapped)
    (while (and (not
                 (setq remapped
                       (cdr (assoc head
                                   inferior-haskell-ghc-internal-ident-alist))))
                (string-match "\\.[^.]+\\'" head))
      (setq tail (concat (match-string 0 head) tail))
      (setq head (substring head 0 (match-beginning 0))))
    (concat (or remapped head) tail)))

;;;###autoload
(defun inferior-haskell-find-haddock (sym)
  "Find and open the Haddock documentation of SYM.
Make sure to load the file into GHCi or Hugs first by using C-c C-l.
Only works for functions in a package installed with ghc-pkg, or
whatever the value of `haskell-package-manager-name' is.

This function needs to find which package a given module belongs
to.  In order to do this, it computes a module-to-package lookup
alist, which is expensive to compute (it takes upwards of five
seconds with more than about thirty installed packages).  As a
result, we cache it across sessions using the cache file
referenced by `inferior-haskell-module-alist-file'. We test to
see if this is newer than `haskell-package-conf-file' every time
we load it."
  (interactive
   (let ((sym (haskell-ident-at-point)))
     (list (read-string (if (> (length sym) 0)
                            (format "Find documentation of (default %s): " sym)
                          "Find documentation of: ")
                        nil nil sym))))
  (setq sym (inferior-haskell-map-internal-ghc-ident sym))
  (let* (;; Find the module and look it up in the alist
         (module (inferior-haskell-get-module sym))
         (alist-record (assoc module (inferior-haskell-module-alist)))
         (package (nth 1 alist-record))
         (file-name (concat (subst-char-in-string ?. ?- module) ".html"))
         (local-path (concat (nth 2 alist-record) "/" file-name))
         (url (if (or (eq inferior-haskell-use-web-docs 'always)
                      (and (not (file-exists-p local-path))
                           (eq inferior-haskell-use-web-docs 'fallback)))
                  (concat inferior-haskell-web-docs-base package "/" file-name
                          ;; Jump to the symbol anchor within Haddock.
                          "#v:" sym)
                (and (file-exists-p local-path)
                     (concat "file://" local-path)))))
    (if url (browse-url url) (error "Local file doesn't exist"))))

(provide 'inf-haskell)

;; arch-tag: 61804287-63dd-4052-bc0e-90f691b34b40
;;; inf-haskell.el ends here
