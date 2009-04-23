;;; multi-term.el --- Multi term buffer.

;; Author: Andy Stewart <lazycat.manatee@gmail.com>
;; Maintainer: Andy Stewart <lazycat.manatee@gmail.com>
;; Copyright (C) 2008, 2009, Andy Stewart, all rights reserved.
;; Created: 2008-09-19 23:02:42
;; Version: 0.7
;; Last-Updated: 2009-01-06 11:00:38
;; URL: http://www.emacswiki.org/emacs/download/multi-term.el
;; Keywords: term, multiple buffer
;; Compatibility: GNU Emacs 23.0.60.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be required by this library:
;;
;;  `term' `cl' `advice'
;;

;;; Installation:
;;
;; Copy multi-term.el to your load-path and add to your ~/.emacs
;;
;;  (require 'multi-term)
;;
;; And setup program that `multi-term' will need:
;;
;; (setq multi-term-program "/bin/bash")
;;
;;      or setup like me "/bin/zsh" ;)
;;
;; Below are the commands you can use:
;;
;;      `multi-term'                    create a new term buffer.
;;      `multi-term-next'               switch to next term buffer.
;;      `multi-term-prev'               switch to previous term buffer.
;;      `multi-term-dedicated-open'     open dedicated term window.
;;      `multi-term-dedicated-close'    close dedicated term window.
;;      `multi-term-dedicated-toggle'   toggle dedicated term window.
;;      `multi-term-dedicated-select'   select dedicated term window.
;;

;;; Customize:
;;
;; `multi-term-program' default is nil, so when creating new term buffer,
;; send environment variable of `SHELL' (`ESHELL', `/bin/sh') to `make-term'.
;;
;; And you can set it to your liking, like me: ;-)
;;
;; (setq multi-term-program "/bin/zsh")
;;
;; `multi-term-default-dir' default is `~/', only use when current buffer
;; is not in a real directory.
;;
;; `multi-term-buffer-name' is the name of term buffer.
;;
;; `multi-term-scroll-show-maximum-output' controls how interpreter
;; output causes window to .
;;
;; `multi-term-scroll-to-bottom-on-output' controls whether interpreter
;; output causes window to scroll.
;;
;; `term-unbind-key-list' is a key list to unbind some keystroke.
;;
;; `term-bind-key-alist' is a key alist that binds some keystroke.
;; If you don't like default, modify it.
;;
;; `multi-term-dedicated-window-height' the height of a dedicated term window.
;;
;; `multi-term-dedicated-max-window-height' the max height limit that dedicated
;; window is allowed.
;;
;; `multi-term-dedicated-skip-other-window-p' whether skip dedicated term
;; window when use command `other-window' to cycle windows order.
;;
;; All of the above can be customize by:
;;      M-x customize-group RET multi-term RET
;;

;;; Commentary:
;;
;; This package is for creating and managing multiple term buffer.
;;
;; Default, `term.el' can create a terminal buffer with `term-mode'.
;; But have some discommoded points:
;;
;; 1 ->
;;      term-mode just create one terminal buffer with `term' command.
;;      But not command for quickly creating and switching with terminal buffers.
;;
;;      Now, use command `multi-term' to quickly create new terminal buffer.
;;      And use command `multi-term-next' or `multi-term-prev' can switch
;;      next or previous terminal buffer quickly, whatever the current buffer.
;;
;; 2 ->
;;      Default, when using *NIX command `exit' from term-mode, will left
;;      an unused buffer.
;;
;;      Now `multi-term' can handled this by closing buffer when `exit' is used.
;;
;; 3 ->
;;      If you use command `kill-this-buffer' to kill terminal buffer forcibly.
;;      term-mode can't interrupt sub-process before kill buffer.
;;
;;      And `multi-term' does this now.
;;
;; 4 ->
;;      And this is most import, `term-mode' is great for using terminal in emacs.
;;      But it's default keystrokes conflict with some global keystrokes (example: C-x).
;;
;;      Now `multi-term' unbinds those conflicting keystrokes with `term-char-mode'.
;;
;; 5 ->
;;      Now `multi-term' can use command `multi-term-dedicated-toggle' to get
;;      a dedicated term window for programming.
;;

;;; Change log:
;;
;; 2009/01/06
;;      * Improve document.
;;
;; 2008/12/29
;;      * Remove option `multi-term-current-window-height' and
;;        function `multi-term-current-directory'.
;;      * Add some functions to make get dedicated term buffer,
;;        those functions is beginning with `multi-term-dedicated-'.
;;      * Modified advice `delete-window', make command `delete-window'
;;        and delete dedicated window, but will remember window height
;;        before deleted.
;;      * Don't remember dedicated window height if larger than max value.
;;      * Fix some bug with `delete-other-windows' and window configuration.
;;        And this bug exists with another extension `sr-speedbar'.
;;      * Add new variable `delete-protected-window-list' for protected
;;        special window that won't be deleted.
;;        This variable is common for any extension that use dedicated
;;        window.
;;      * Fix doc.
;;
;; 2008/12/21
;;      * Default bind `C-m' with `term-send-input'.
;;
;; 2008/12/10
;;      * Improve customize interface.
;;      * Setup customize automatically, don't need to user setup it up.
;;      * Add option `multi-term-try-create'.
;;      * Make function `multi-term-switch' accept offset argument.
;;      * Fix doc.
;;
;; 2008/10/22
;;      * Add variable `multi-term-current-window-height'.
;;      * Add variable `multi-term-buffer-name'.
;;      * Add variable `term-unbind-key-list'.
;;      * Add variable `term-rebind-key-alist'.
;;      * Move key setup and some extension from `term-extension.el'.
;;      * Create new function `multi-term-keystroke-setup'.
;;      * Fix doc.
;;
;; 2008/09/19
;;      * First released.
;;

;;; Acknowledgments:
;;
;;      Mark Triggs     <mst@dishevelled.net>
;;              For create multi-shell.el
;;      Aaron S. Hawley <aaron.s.hawley@gmail.com>
;;              For improve document.
;;

;;; TODO
;;
;; None
;;

;;; Require:
(require 'term)
(require 'cl)
(require 'advice)

;;; Code:

;;; Customize

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup multi-term nil
  "Multi term manager."
  :group 'term)

(defcustom multi-term-program nil
  "The program of term.
If this is nil, setup to environment variable of `SHELL'."
  :type 'string
  :group 'multi-term)

(defcustom multi-term-try-create t
  "Try to create a new term buffer when switch.

When use `multi-term-next' or `multi-term-prev', switch term buffer,
and try to create a new term buffer if no term buffers exist."
  :type 'boolean
  :group 'multi-shell)

(defcustom multi-term-default-dir "~/"
  "The default directory for terms if current directory doesn't exist."
  :type 'string
  :group 'multi-term)

(defcustom multi-term-buffer-name "terminal"
  "The buffer name of term buffer."
  :type 'string
  :group 'multi-term)

(defcustom multi-term-scroll-show-maximum-output nil
  "*Controls how interpreter output causes window to scroll.
If non-nil, then show the maximum output when the window is scrolled.

See variable `multi-term-scroll-to-bottom-on-output'."
  :type 'boolean
  :group 'multi-term)

(defcustom multi-term-scroll-to-bottom-on-output nil
  "*Controls whether interpreter output causes window to scroll.
If nil, then do not scroll.  If t or `all', scroll all windows showing buffer.
If `this', scroll only the selected window.
If `others', scroll only those that are not the selected window.

The default is nil.

See variable `multi-term-scroll-show-maximum-output'."
  :type 'boolean
  :group 'multi-term)

(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'multi-term)

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-raw)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("M-," . term-send-input)
    ("M-." . comint-dynamic-complete))
  "The key alist that will need to be bind.
If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'multi-term)

(defcustom multi-term-dedicated-window-height 14
  "The height of `multi-term' dedicated window."
  :type 'integer
  :group 'multi-term)

(defcustom multi-term-dedicated-max-window-height 30
  "The max height limit of `multi-term' dedicated window.
Default, when hide `multi-term' dedicated window, will remember
window height before hide, except height is larger than this.`"
  :type 'integer
  :group 'multi-term)

(defcustom multi-term-dedicated-skip-other-window-p nil
  "Default, can have `other-window' select window in cyclic ordering of windows.
In cases you don't want to select `multi-term' dedicated window, use `other-window'
and make `multi-term' dedicated window as a viewable sidebar.

So please turn on this option if you want to skip `multi-term' dedicated window with `other-window'.

Default is nil."
  :type 'boolean
  :set (lambda (symbol value)
         (set symbol value)
         (when (ad-advised-definition-p 'other-window)
           (multi-term-dedicated-handle-other-window-advice value)))
  :group 'multi-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Constant ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst multi-term-dedicated-buffer-name "MULTI-TERM-DEDICATED"
  "The buffer name of dedicated `multi-term'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar multi-term-dedicated-window nil
  "The dedicated `multi-term' window.")

(defvar multi-term-dedicated-buffer nil
  "The dedicated `multi-term' buffer.")

(defvar delete-protected-window-list '()
  "A list that contains delete-protected windows
to avoid deletion in `delete-other-windows' advice.

And this variable is common variable for many similar
advice for `delete-other-windows'.  So, you can
define this variable in another package, and add
protected windows to avoid having many
similar advice for same function that conflict with each other.")

(add-to-list 'delete-protected-window-list 'multi-term-dedicated-window t 'eq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun multi-term ()
  "Create new term buffer."
  (interactive)
  (let (term-buffer)
    ;; Set buffer.
    (setq term-buffer (multi-term-get-buffer))
    (set-buffer term-buffer)
    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)
    ;; Switch buffer
    (switch-to-buffer term-buffer)))

(defun multi-term-next (&optional offset)
  "Go to the next term."
  (interactive "P")
  (multi-term-switch 'NEXT (or offset 1)))

(defun multi-term-prev (&optional offset)
  "Go to the previous term."
  (interactive "P")
  (multi-term-switch 'PREVIOUS (or offset 1)))

(defun multi-term-dedicated-open ()
  "Open dedicated `multi-term' window."
  (interactive)
  (if (not (multi-term-dedicated-exist-p))
      (let ((current-window (selected-window)))
        (if (multi-term-buffer-exist-p multi-term-dedicated-buffer)
            (unless (multi-term-window-exist-p multi-term-dedicated-window)
              (multi-term-dedicated-get-window))
          (multi-term-dedicated-get-window)
          ;; Set buffer.
          (setq multi-term-dedicated-buffer (multi-term-get-buffer t))
          (set-buffer (multi-term-dedicated-get-buffer-name))
          ;; Whether skip `other-window'.
          (multi-term-dedicated-handle-other-window-advice multi-term-dedicated-skip-other-window-p)
          ;; Internal handle for `multi-term' buffer.
          (multi-term-internal))
        (set-window-buffer multi-term-dedicated-window (get-buffer (multi-term-dedicated-get-buffer-name)))
        (set-window-dedicated-p multi-term-dedicated-window t)
        (select-window current-window))
    (message "`multi-term' dedicated window has exist.")))

(defun multi-term-dedicated-close ()
  "Close dedicated `multi-term' window."
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (let ((current-window (selected-window)))
        (multi-term-dedicated-select)
        ;; Remember height.
        (multi-term-dedicated-remember-window-height)
        ;; Delete dedicated window.
        (delete-window multi-term-dedicated-window)
        (if (multi-term-window-exist-p current-window)
            (select-window current-window)))
    (message "`multi-term' window is not exist.")))

(defun multi-term-dedicated-remember-window-height ()
  "Remember window height."
  (let ((win-height (multi-term-current-window-take-height)))
    (if (and (multi-term-dedicated-window-p) ;in `multi-term' window
             (> win-height 1)
             (<= win-height multi-term-dedicated-max-window-height))
        (setq multi-term-dedicated-window-height win-height))))

(defun multi-term-dedicated-toggle ()
  "Toggle dedicated `multi-term' window."
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (multi-term-dedicated-close)
    (multi-term-dedicated-open)))

(defun multi-term-dedicated-select ()
  "Select the `multi-term' dedicated window."
  (interactive)
  (if (multi-term-dedicated-exist-p)
      (select-window multi-term-dedicated-window)
    (message "`multi-term' window is not exist.")))

(defun term-send-backward-kill-word ()
  "Backward kill word in term mode."
  (interactive)
  (term-send-raw-string "\C-w"))

(defun term-send-forward-kill-word ()
  "Kill word in term mode."
  (interactive)
  (term-send-raw-string "\ed"))

(defun term-send-backward-word ()
  "Move backward word in term mode."
  (interactive)
  (term-send-raw-string "\eb"))

(defun term-send-forward-word ()
  "Move forward word in term mode."
  (interactive)
  (term-send-raw-string "\ef"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilise Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun multi-term-internal ()
  "Internal handle for `multi-term' buffer."
  ;; Add customize keystroke with `term-mode-hook'
  (remove-hook 'term-mode-hook 'multi-term-keystroke-setup)
  (add-hook 'term-mode-hook 'multi-term-keystroke-setup)
  ;; Load term mode
  (term-mode)
  (term-char-mode)
  ;; Handle term buffer close
;;  (multi-term-handle-close)
  ;; Handle `output' variable.
  (setq term-scroll-show-maximum-output multi-term-scroll-show-maximum-output
        term-scroll-to-bottom-on-output multi-term-scroll-to-bottom-on-output)
  ;; Add hook to be sure `term' interrupt subjob before buffer killed.
;;  (add-hook 'kill-buffer-hook 'multi-term-kill-buffer-hook)
  )

(defun multi-term-get-buffer (&optional dedicated-window)
  "Get term buffer.
Create dedicated `multi-term' window if option `dedicated-window' is `non-nil'."
  (with-temp-buffer
    (let (term-list-length              ;get length of term list
          index                         ;setup new term index
          term-name)                    ;term name
      (if dedicated-window
          (setq term-name multi-term-dedicated-buffer-name)
        ;; Compute index.
        (setq term-list-length (length (multi-term-list)))
        (setq index (if term-list-length (1+ term-list-length) 1))
        ;; switch to current local directory,
        ;; if in-existence, switch to `multi-term-default-dir'.
        (cd (or default-directory (expand-file-name multi-term-default-dir)))
        ;; adjust value N when max index of term buffer is less than length of term list
        (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-term-buffer-name index)))
          (setq index (1+ index)))
        (setq term-name (format "%s<%s>" multi-term-buffer-name index)))
      ;; make term, details to see function `make-term' in `term.el'.
      (make-term
       term-name
       (or multi-term-program
           (getenv "SHELL")
           (getenv "ESHELL")
           "/bin/sh")))))

(defun multi-term-handle-close ()
  "Close current term buffer when `exit' from term buffer."
  (when (ignore-errors (get-buffer-process (current-buffer)))
    (set-process-sentinel (get-buffer-process (current-buffer))
                          (lambda (proc change)
                            (when (string-match "\\(finished\\|exited\\)" change)
                              (kill-buffer (process-buffer proc)))))))

(defun multi-term-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  ;; Remember dedicated window height.
  (multi-term-dedicated-remember-window-height)
  ;; Interrupt the current subjob
  ;; when have alive process with current term buffer
  (when (and (eq major-mode 'term-mode)
             (term-check-proc (current-buffer)))
    ;; Interrupt sub-process.
    (term-interrupt-subjob)))

(defun multi-term-list ()
  "List term buffers presently active."
  ;; Autload command `remove-if-not'.
  (autoload 'remove-if-not "cl-seq")
  (sort
   (remove-if-not (lambda (b)
                    (string-match
                     (concat "^\*" multi-term-buffer-name)
                     (buffer-name b)))
                  (buffer-list))
   (lambda (a b)
     (< (string-to-number
         (cadr (split-string (buffer-name a) "[<>]")))
        (string-to-number
         (cadr (split-string (buffer-name b)  "[<>]")))))))

(defun multi-term-switch (direction offset)
  "If DIRECTION is `NEXT', switch to the next term.
If `PREVIOUS', switch to the previous term."
  (let (terms this-buffer)
    (setq terms (multi-term-list))
    (if (consp terms)
        (progn
          (setf (cdr (last terms)) terms)
          (setq this-buffer (position (current-buffer) (multi-term-list)))
          (if this-buffer
              (if (eql direction 'NEXT)
                  (switch-to-buffer (nth (+ this-buffer offset) terms))
                (switch-to-buffer (nth (+ (- (length (multi-term-list)) offset)
                                          this-buffer) terms)))
            (switch-to-buffer (car terms))))
      (if multi-term-try-create
          (progn
            (multi-term)
            (message "Create a new `multi-term' buffer."))
        (message "Haven't any `multi-term' buffer exist.")))))

(defun multi-term-keystroke-setup ()
  "Keystroke setup of `term-char-mode'.

By default, the key bindings of `term-char-mode' conflict with user's keystroke.
So this function unbinds some keys with `term-raw-map',
and binds some keystroke with `term-raw-map'."
  (let (bind-key bind-command)
    ;; Unbind base key that conflict with user's keys-tokes.
    (dolist (unbind-key term-unbind-key-list)
      (cond
       ((stringp unbind-key) (setq unbind-key (read-kbd-macro unbind-key)))
       ((vectorp unbind-key) nil)
       (t (signal 'wrong-type-argument (list 'array unbind-key))))
      (define-key term-raw-map unbind-key nil))
    ;; Add some i use keys.
    ;; If you don't like my keystroke,
    ;; just modified `term-bind-key-alist'
    (dolist (element term-bind-key-alist)
      (setq bind-key (car element))
      (setq bind-command (cdr element))
      (cond
       ((stringp bind-key) (setq bind-key (read-kbd-macro bind-key)))
       ((vectorp bind-key) nil)
       (t (signal 'wrong-type-argument (list 'array bind-key))))
      (define-key term-raw-map bind-key bind-command))))

(defun multi-term-dedicated-handle-other-window-advice (activate)
  "Handle advice for function `other-window'."
  (if activate
      (ad-enable-advice 'other-window 'after 'multi-term-dedicated-other-window-advice)
    (ad-disable-advice 'other-window 'after 'multi-term-dedicated-other-window-advice))
  (ad-activate 'other-window))

(defun multi-term-current-window-take-height (&optional window)
  "Return the height the `window' takes up.
Not the value of `window-height', it returns usable rows available for `window'
If `window' is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 3 edges) (nth 1 edges))))

(defun multi-term-dedicated-get-window ()
  "Get `multi-term' dedicated window."
  (setq multi-term-dedicated-window
        (split-window
         (selected-window)
         (- (multi-term-current-window-take-height) multi-term-dedicated-window-height))))

(defun multi-term-dedicated-get-buffer-name ()
  "Get the buffer name of `multi-term' dedicated window."
  (format "*%s*" multi-term-dedicated-buffer-name))

(defun multi-term-dedicated-exist-p ()
  "Return `non-nil' if `multi-term' dedicated window exist."
  (and (multi-term-buffer-exist-p multi-term-dedicated-buffer)
       (multi-term-window-exist-p multi-term-dedicated-window)))

(defun multi-term-window-exist-p (window)
  "Return `non-nil' if `window' exist.
Otherwise return `nil'."
  (and window (window-live-p window)))

(defun multi-term-buffer-exist-p (buffer)
  "Return `non-nil' if `buffer' exist.
Otherwise return `nil'."
  (and buffer (buffer-live-p buffer)))

(defun multi-term-dedicated-window-p ()
  "Return `non-nil' if current window is `multi-term' dedicated window.
Otherwise return `nil'."
  (equal (multi-term-dedicated-get-buffer-name) (buffer-name (window-buffer))))

(defun multi-term-two-windows-p ()
  "Return `non-nil' if just have two windows in current frame."
  (let ((base-window (selected-window))
        next-base-window)
    (if (eq base-window (minibuffer-window)) ;select next window if in `minibuffer'
        (setq base-window (next-window base-window)))
    (setq next-base-window (next-window base-window))
    (and (not (eq base-window next-base-window))         ;not just one window
         (eq base-window (next-window next-base-window)) ;two window equal each other
         )))

(defun multi-term-dedicated-match-protected-window-p (win)
  "Return `non-nil' if match `win' in `delete-window-protected-window-list'.
Otherwise, return `nil'."
  (let (match-protected-window-p)
    (catch 'match
      (dolist (protected-window delete-protected-window-list)
        (when (eq (eval protected-window) win)
          (setq match-protected-window-p t)
          (throw 'match "Match one protected window."))))
    match-protected-window-p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advice ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice delete-other-windows (around multi-term-delete-other-window-advice activate)
  "This is advice to make `multi-term' a dedicated window that can't deleted by
command `delete-other-windows'."
  (let ((multi-term-dedicated-active-p (multi-term-window-exist-p multi-term-dedicated-window)))
    (if multi-term-dedicated-active-p
        (let ((current-window (selected-window)))
          (dolist (win (window-list))
            (when (and (window-live-p win)
                       (not (eq current-window win))
                       (not (multi-term-dedicated-match-protected-window-p win)))
              (delete-window win))))
      ad-do-it)))

(defadvice delete-window (before multi-term-delete-window-advice activate)
  "Use `delete-window' delete `multi-term' dedicated window have same effect as
`multi-term-dedicated-close'.
This advice to remember `multi-term' dedicated window height before deleting."
  ;; Remember window height before deleted.
  (multi-term-dedicated-remember-window-height))

(defadvice pop-to-buffer (before multi-term-pop-to-buffer-advice activate)
  "By default, function `display-buffer' can't display buffer in selected window
if current window is `dedicated'.

So function `display-buffer' conflicts with `sr-speedbar' window, because
`sr-speedbar' window is a `dedicated' window.

That is to say, when current frame is two windows: `multi-term' dedicated window
and another window, any functions that uses `display-buffer' can't split windows
to display buffer, even when the option `pop-up-windows' is enabled.

And the example function that can induce the problem is `pop-to-buffer'.

This advice will fix this problem when current frame just have `multi-term'
dedicated window and another window."
  (when (and pop-up-windows             ;`pop-up-windows' is enable
             (multi-term-two-windows-p) ;just have two window in current frame
             (multi-term-window-exist-p multi-term-dedicated-window)
             (not (multi-term-dedicated-window-p))) ;not in `sr-speedbar' window
    (split-window-vertically)
    (windmove-down)))

(defadvice other-window (after multi-term-dedicated-other-window-advice)
  "Default, can use `other-window' select window in cyclic ordering of windows.
But sometimes we don't want to select `sr-speedbar' window, but use `other-window' and
just make `multi-term' dedicated window as a viewable sidebar.

This advice can make `other-window' skip `multi-term' dedicated window."
  (let ((count (or (ad-get-arg 0) 1)))
    (when (and (multi-term-window-exist-p multi-term-dedicated-window)
               (eq multi-term-dedicated-window (selected-window)))
      (other-window count))))

(provide 'multi-term)

;;; multi-term.el ends here
