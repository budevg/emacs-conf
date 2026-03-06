;;; agent-shell-diff.el --- A quick way to query/display a diff. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Alvaro Ramirez

;; Author: Alvaro Ramirez https://xenodium.com
;; URL: https://github.com/xenodium/agent-shell

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Report issues at https://github.com/xenodium/agent-shell/issues
;;
;; ✨ Please support this work https://github.com/sponsors/xenodium ✨

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'diff)
(require 'diff-mode)

(defvar-local agent-shell-on-exit nil
  "Function to call when the diff buffer is killed.

This variable is automatically set by :on-exit from `agent-shell-diff'
and can be temporarily let-bound to nil to prevent the
on-exit callback from running when the buffer is killed.")

(defvar-local agent-shell-diff--file nil
  "Buffer-local file path associated with the diff.")

(defvar-local agent-shell-diff--accept-all-command nil
  "Buffer-local command to accept all changes in the diff.")

(defvar-local agent-shell-diff--reject-all-command nil
  "Buffer-local command to reject all changes in the diff.")

(defvar agent-shell-diff-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'diff-hunk-next)
    (define-key map (kbd "p") #'diff-hunk-prev)
    (define-key map (kbd "y") #'agent-shell-diff-accept-all)
    (define-key map (kbd "C-c C-c") #'agent-shell-diff-reject-all)
    (define-key map (kbd "f") #'agent-shell-diff-open-file)
    (define-key map (kbd "q") #'kill-current-buffer)
    map)
  "Keymap for `agent-shell-diff-mode'.")

(define-derived-mode agent-shell-diff-mode diff-mode "Agent-Shell-Diff"
  "Major mode for `agent-shell' diff buffers.
Derives from `diff-mode'.  Provides `agent-shell-diff-accept-all'
and `agent-shell-diff-reject-all' commands that can be rebound
via `agent-shell-diff-mode-map'."
  :group 'agent-shell
  ;; Don't inherit diff-mode-map (some bindings can be destructive).
  (set-keymap-parent agent-shell-diff-mode-map nil)
  (setq buffer-read-only t))

(defun agent-shell-diff-accept-all ()
  "Accept all changes in the current diff buffer."
  (interactive)
  (if agent-shell-diff--accept-all-command
      (funcall agent-shell-diff--accept-all-command)
    (user-error "No accept command available in this buffer")))

(defun agent-shell-diff-reject-all ()
  "Reject all changes in the current diff buffer."
  (interactive)
  (if agent-shell-diff--reject-all-command
      (funcall agent-shell-diff--reject-all-command)
    (user-error "No reject command available in this buffer")))

(cl-defun agent-shell-diff (&key old new on-exit on-accept on-reject title file)
  "Display a diff between OLD and NEW strings in a buffer.

Creates a new buffer showing the differences between OLD and NEW
using `agent-shell-diff-mode'.  The buffer is read-only.

When the buffer is killed, calls ON-EXIT with no arguments.

Arguments:
  :OLD       - Original string content
  :NEW       - Modified string content
  :ON-EXIT   - Function called with no arguments when buffer is killed
  :ON-ACCEPT - Command to accept all changes
  :ON-REJECT - Command to reject all changes
  :TITLE     - Optional title to display in header line
  :FILE      - File path"
  (let* ((diff-buffer (generate-new-buffer "*agent-shell-diff*"))
         (calling-window (selected-window))
         (calling-buffer (current-buffer))
         (interrupt-key (where-is-internal 'agent-shell-interrupt
                                           (current-local-map) t)))
    (unwind-protect
        (progn
          (with-current-buffer diff-buffer
            (let ((inhibit-read-only t)
                  (diff-mode-read-only nil))
              (erase-buffer)
              ;; Set mode before inserting diff so diff-no-select
              ;; doesn't reset font-lock (see #316).
              (agent-shell-diff-mode)
              (agent-shell-diff--insert-diff old new file diff-buffer)
              ;; Add overlays to hide scary text.
              (save-excursion
                (goto-char (point-min))
                ;; Remove command added by diff-no-select
                (delete-region (point) (progn (forward-line 1) (point)))
                ;; Remove "Diff finished." added by diff-no-select
                (delete-region (progn (goto-char (point-max)) (forward-line -1) (forward-line 0) (point))
                               (point-max))
                (goto-char (point-min))
                ;; Hide --- and +++ lines
                (while (re-search-forward "^\\(---\\|\\+\\+\\+\\).*\n" nil t)
                  (let ((overlay (make-overlay (match-beginning 0) (match-end 0))))
                    (overlay-put overlay 'category 'diff-header)
                    (overlay-put overlay 'display "")
                    (overlay-put overlay 'evaporate t)))
                ;; Replace @@ lines with "Changes"
                (goto-char (point-min))
                (while (re-search-forward "^@@.*@@.*\n" nil t)
                  (let ((overlay (make-overlay (match-beginning 0) (match-end 0)))
                        (face 'diff-hunk-header))  ; or any face you prefer
                    (overlay-put overlay 'category 'diff-header)
                    ;; Intended display is:
                    ;; ╭─────────╮
                    ;; │ changes │
                    ;; ╰─────────╯
                    ;; Using before-string so diff-hunk-next
                    ;; lands on "│" instead of "╭".
                    (overlay-put overlay 'before-string
                                 (propertize "\n╭─────────╮\n" 'face face))
                    (overlay-put overlay 'display
                                 (propertize "│ changes │\n╰─────────╯\n\n" 'face face))
                    (overlay-put overlay 'evaporate t)))))
            (goto-char (point-min))
            (ignore-errors (diff-hunk-next))
            (setq agent-shell-diff--file file
                  agent-shell-diff--accept-all-command on-accept
                  agent-shell-diff--reject-all-command on-reject)
            (when on-exit
              (setq agent-shell-on-exit on-exit)
              (add-hook 'kill-buffer-hook
                        (lambda ()
                          (with-current-buffer diff-buffer
                            (when agent-shell-on-exit
                              (with-current-buffer calling-buffer
                                (funcall on-exit))))
                          (with-current-buffer calling-buffer
                            ;; Make sure give focus back to calling buffer on exit.
                            (if (window-live-p calling-window)
                                (if (eq (window-buffer calling-window) calling-buffer)
                                    ;; Calling buffer still on calling window, just select it.
                                    (select-window calling-window)
                                  ;; Calling buffer not on calling window, restore it.
                                  (progn
                                    (set-window-buffer calling-window calling-buffer)
                                    (select-window calling-window))))))
                        nil t))
            (let ((map (copy-keymap agent-shell-diff-mode-map)))
              (when (and interrupt-key
                         (not (lookup-key map interrupt-key)))
                (define-key map interrupt-key #'agent-shell-diff-reject-all))
              (use-local-map map))
            (setq header-line-format
                  (substitute-command-keys
                   (concat
                    "  "
                    (when title
                      (concat (propertize title 'face 'mode-line-emphasis) " "))
                    "\\[diff-hunk-next] next hunk  "
                    "\\[diff-hunk-prev] previous hunk  "
                    "\\[agent-shell-diff-accept-all] accept  "
                    "\\[agent-shell-diff-reject-all] reject  "
                    "\\[agent-shell-diff-open-file] open  "
                    "\\[kill-current-buffer] quit")))))
      (pop-to-buffer diff-buffer '((display-buffer-use-some-window
                                    display-buffer-same-window))))))

(defun agent-shell-diff-open-file ()
  "Open the file associated with the current diff buffer."
  (interactive)
  (if agent-shell-diff--file
      (find-file agent-shell-diff--file)
    (user-error "No file associated with this diff buffer")))

(defun agent-shell-diff--insert-diff (old new file buf)
  "Insert diff from FILE between OLD and NEW strings in buffer BUF."
  (let* ((suffix (format ".%s" (file-name-extension file)))
         (old-file (make-temp-file "old" nil suffix))
         (new-file (make-temp-file "new" nil suffix)))
    (with-temp-file old-file (insert old))
    (with-temp-file new-file (insert new))
    (diff-no-select old-file new-file "-U3" t buf)))

(provide 'agent-shell-diff)

;;; agent-shell-diff.el ends here
