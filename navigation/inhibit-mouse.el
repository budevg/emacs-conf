;;; inhibit-mouse.el --- Deactivate mouse input during editing -*- lexical-binding: t; -*-

;; Copyright (C) 2024 James Cherti | https://www.jamescherti.com/contact/

;; Author: James Cherti
;; Version: 1.0.0
;; URL: https://github.com/jamescherti/inhibit-mouse.el
;; Keywords: convenience
;; Package-Requires: ((emacs "24.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The inhibit-mouse package allows the disabling of mouse input in Emacs using
;; inhibit-mouse-mode.
;;
;; Instead of modifying the keymap of its own mode (as the disable-mouse package
;; does), enabling inhibit-mouse-mode only modifies input-decode-map to disable
;; mouse events, making it more efficient and faster than disable-mouse.
;;
;; Additionally, the inhibit-mouse package allows for the restoration of mouse
;; input when inhibit-mouse-mode is disabled.

;;; Code:

(defgroup inhibit-mouse nil
  "Non-nil if inhibit-mouse mode mode is enabled."
  :group 'inhibit-mouse
  :prefix "inhibit-mouse-"
  :link '(url-link
          :tag "Github"
          "https://github.com/jamescherti/inhibit-mouse.el"))

(defcustom inhibit-mouse-mode-lighter " InhibitMouse"
  "Mode-line lighter for `inhibit-mouse-mode'."
  :group 'inhibit-mouse
  :type 'string)

(defvar inhibit-mouse-button-numbers '(1 2 3 4 5 6 7 8))

(defvar inhibit-mouse-button-events
  '("mouse"
    "up-mouse"
    "down-mouse"
    "drag-mouse")
  "List of mouse button events to be inhibited.")

(defvar inhibit-mouse-misc-events
  '("wheel-up"
    "wheel-down"
    "wheel-left"
    "wheel-right"
    "pinch")
  "List of miscellaneous mouse events to be inhibited.")

(defvar inhibit-mouse-multipliers
  '("double" "triple")
  "List of mouse multiplier events to be inhibited.")

(defvar inhibit-mouse-key-modifiers
  '((control)
    (meta)
    (shift)
    (control meta shift)
    (control meta)
    (control shift)
    (meta shift))
  "List of key modifier combinations to be inhibited for mouse events.")

(defvar inhibit-mouse--ignored-events nil
  "The mouse events that have been ignored. This is an internal variable.")

(defun inhibit-mouse--define-input-event (modifiers base value)
  "Suppress a specific input event.

This function disables an input event defined by the combination of
MODIFIERS and BASE, modifying the `input-decode-map` to ensure that
the specified event is not processed or is remapped to a specified VALUE.

MODIFIERS: A list of modifier keys as symbols (e.g., (control meta)).
BASE: The base input event (e.g., wheel-up) to be suppressed.
VALUE: The value to associate with the suppressed input event, which can
       be nil to ignore the event or another function or command to remap it.

The function is useful for disabling or remapping unwanted mouse events
during editing or other operations, allowing users to maintain focus on
keyboard input without interruption from mouse actions."
  (when value
    (push (cons modifiers base) inhibit-mouse--ignored-events))
  (define-key input-decode-map
              (vector (event-convert-list (append modifiers (list base))))
              value))

;;;###autoload
(define-minor-mode inhibit-mouse-mode
  "Toggle `inhibit-mouse-mode'."
  :global t
  :lighter inhibit-mouse-mode-lighter
  :group 'inhibit-mouse
  (if inhibit-mouse-mode
      ;; ENABLE: inhibit-mouse-mode
      (progn
        (setq inhibit-mouse--ignored-events nil)

        (dolist (modifiers (append (list nil) inhibit-mouse-key-modifiers))
          (dolist (base inhibit-mouse-misc-events)
            (inhibit-mouse--define-input-event modifiers
                                               (intern base)
                                               (lambda (_prompt) [])))

          (dolist (multiplier (cons nil inhibit-mouse-multipliers))
            (dolist (button inhibit-mouse-button-numbers)
              (dolist (event inhibit-mouse-button-events)
                (let ((base (format "%s%s-%d"
                                    (if multiplier
                                        (concat multiplier "-")
                                      "")
                                    event
                                    button)))
                  (inhibit-mouse--define-input-event
                   modifiers
                   (intern base)
                   (lambda (_prompt) []))))))))
    ;; DISABLE: inhibit-mouse-mode
    (dolist (ignored-event inhibit-mouse--ignored-events)
      (let ((modifier (car ignored-event))
            (base (cdr ignored-event)))
        ;; Remove the ignored events when disabling the mode
        (inhibit-mouse--define-input-event modifier base nil)))

    ;; Clear the list after restoring
    (setq inhibit-mouse--ignored-events nil)))

(provide 'inhibit-mouse)
;;; inhibit-mouse.el ends here
