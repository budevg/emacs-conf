;;; sr-speedbar.el --- Same frame speedbar

;; Author: Sebastain Rose <sebastian_rose@gmx.de>
;; Maintainer: Sebastain Rose <sebastian_rose@gmx.de>
;; Copyright (C) 2008, Sebastain Rose, all rights reserved.
;; Created: 2008
;; Version: 0.0.4
;; Last-Updated: 2008-09-28 22:13:50
;; URL: http://www.emacswiki.org/cgi-bin/wiki/sr-speedbar.el
;; Keywords: speedbar
;; Compatibility: GNU Emacs 23.0.60.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;; Features that might be requried by this library:
;;
;;  `speedbar'
;;

;;; Installation:
;;
;; Copy sr-speedbar.el to your load-path and add to your ~/.emacs
;;
;;  (require 'sr-speedbar)
;;  (global-set-key [(super ?s)] 'sr-speedbar-togge)
;;
;; ... or any key binding you like.
;;

;;; Commentary:
;;
;; The sr-speedbar.el was created just because I could not believe what I
;; read on http://www.emacswiki.org/cgi-bin/wiki/Speedbar. They wrote there
;; that it is not possible to show the speedbar in the same frame. But, as
;; we all know, ecb had this already. So I started as some kind of joke :)
;; But when I found it useful and use it all the time.
;;
;; Now your windows key and 's' show the speedbar in an extra window, same
;; frame. Currently speedbar always shows up on the right. You can customize
;; the initial width of the speedbar window further down for console/DOS and
;; X/Win/MacOS seperatly.
;;
;; Enjoy!
;;

;;; Change log:
;;
;;  * 28 SEP 2008:
;;     * Andy Stewart:
;;              Fix a bug, when `sr-speedbar-toggle' many times, window width
;;              will increment automatically.
;;
;;              Use around advices replace, make code simple.
;;
;;              Use `sr-speedbar-open' replace `sr-speedbar-no-separate-frame'.
;;
;;              Clean up code.
;;
;;     * Sebastian:
;;              set `sr-speedbar-delete-windows' to nil to avoid
;;              the removal of other windows.
;;
;;  * 26 JUN 2008:
;;     * Added Andy Stewart's patch to refresh the speedbar's contents.
;;       Thank's for this one!
;;
;; * Added some lines to get it working:
;;     * splitting the window and remember it,
;;     * changing the way speedbar finds a file.
;;     * File view of speedbar is now working all right.
;;     * C-x 1 in other window deletes speedbar-window, just calling
;;       M-x sr-speedbar-no-separate-frame again is fine now.
;;     * Toggle speedbar works, width is save when toggeling.
;;     * Recalc speedbar width if window-width - speedbar-width <= 0
;;     * Speedbar window is now dedicated to speedbar-buffer.
;;

;;; Acknowledgements:
;;
;;      All emacsers ... :)
;;

;;; TODO
;;
;;
;;

;;; Require
(require 'speedbar)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; User Customization ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sr-speedbar-width-x 24
  "Initial width of sr-speedbar-window under window system.")

(defvar sr-speedbar-width-console 24
  "Initial width of sr-speedbar-window on console.")

(defvar sr-speedbar-delete-windows nil
  "Allow the speedbar to delte other windows before showing up.
If nil, speedbar will not touch your window configuration.
Otherwise delete-other-windows will be called before showing
the speedbar.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst sr-speedbar-version "0.0.4"
  "Current version")

(defvar sr-speedbar-width nil
  "Intial width of speedbar-window")

(defconst sr-speedbar-buffer-name "*SPEEDBAR*"
  "The buffer name of sr-speedbar.")

(defvar sr-speedbar-window nil
  "Speedbar window")

(defvar sr-speedbar-last-refresh-dictionary nil
  "The last refresh dictionary record of 'sr-speedbar-refresh'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sr-speedbar-toggle ()
  "Toggle visibility of sr-speedbar by resizing the sr-speedbar-window to a minimal width
or the last width when visible. Use this function to create or toggle visibility
of a speedbar-window. It will be created if neccessary."
  (interactive)
  (if (and (window-live-p sr-speedbar-window) ;when `sr-speedbar-window' visible
           (buffer-live-p speedbar-buffer))   ;and contain `speedbar-buffer'
      (sr-speedbar-close)                     ;close `sr-speedbar' window
    (sr-speedbar-open)                        ;otherwise, open `sr-speedbar' window in same frame
    ))

(defun sr-speedbar-open ()
  "Create `sr-speedbar' window."
  (interactive)
  (let ((current-window (selected-window)))
    (if sr-speedbar-delete-windows      ;if `sr-speedbar-delete-windows' is non-nil
        (delete-other-windows))         ;ensure only one window is there
    (if (not (buffer-live-p speedbar-buffer))
        (progn
          (if (<= (current-window-take-width) sr-speedbar-width) ;if current window width is narrower than `sr-speedbar-width'
              (sr-speedbar-recalculate-width))                   ;recalculate width of `sr-speedbar'
          (sr-speedbar-get-window)      ;get `sr-speedbar' window that split current window
          (setq speedbar-buffer (get-buffer-create sr-speedbar-buffer-name)
                speedbar-frame (selected-frame)
                dframe-attached-frame (selected-frame)
                speedbar-select-frame-method 'attached
                speedbar-verbosity-level 0 ;don't say anything, i don't like ... :)
                speedbar-last-selected-file nil)
          (set-buffer speedbar-buffer)
          (buffer-disable-undo speedbar-buffer) ;make disable in speedbar buffer, otherwise will occur `undo-outer-limit' error
          (speedbar-mode)
          (speedbar-reconfigure-keymaps)
          (speedbar-update-contents)
          (speedbar-set-timer 1)
          (add-hook 'speedbar-before-visiting-file-hook 'sr-speedbar-before-visiting-file-hook t)
          (add-hook 'speedbar-before-visiting-tag-hook 'sr-speedbar-before-visiting-tag-hook t)
          (add-hook 'speedbar-visiting-file-hook 'sr-speedbar-visiting-file-hook t)
          (add-hook 'speedbar-visiting-tag-hook 'sr-speedbar-visiting-tag-hook t)
          (add-hook 'kill-buffer-hook 'sr-speedbar-kill-buffer-hook) ;add `kill-buffer-hook'
          ;; (make-local-hook 'kill-buffer-hook) ; depricated. uncomment for emacs 21
          )
      (if (not (window-live-p sr-speedbar-window))
          (sr-speedbar-get-window)      ;get `sr-speedbar' window that split current window
        ))
    (set-window-buffer sr-speedbar-window (get-buffer sr-speedbar-buffer-name))
    (set-window-dedicated-p sr-speedbar-window t) ;make `sr-speedbar-window' dedicated to speedbar-buffer.
    (select-window current-window)
    (bury-buffer speedbar-buffer)
    ))

(defun sr-speedbar-close ()
  "Close `sr-speedbar' window and save window width."
  (interactive)
  (let ((current-window (selected-window))) ;if not this, when you `sr-speedbar-toggle' in `sr-speedbar' buffer
                                        ;will bury emacs ... :)
    (select-window sr-speedbar-window)
    (if (> (current-window-take-width) 1) ;if width of `sr-speedbar-window' is a valid value
        (setq sr-speedbar-width (current-window-take-width))) ;remember it make next restore same width
    (bury-buffer)
    (if (window-live-p current-window)
        (select-window current-window))))

(defun sr-speedbar-select-window ()
  "Force the windows that contain `sr-speedbar'."
  (interactive)
  (select-window sr-speedbar-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utils functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sr-speedbar-recalculate-width ()
  "Calculate the speedbar width with respect of window system"
  (if (and window-system
           (not (string= "pc" window-system)))
      (setq sr-speedbar-width sr-speedbar-width-x)
    (setq sr-speedbar-width sr-speedbar-width-console)))
(or sr-speedbar-width (sr-speedbar-recalculate-width)) ;init calculate-width

(defun sr-speedbar-get-window ()
  "Get `sr-speedbar' window."
  (setq sr-speedbar-window (split-window
                            (selected-window)
                            (- (current-window-take-width) sr-speedbar-width)
                            t)))

(defun sr-speedbar-before-visiting-file-hook ()
  "Function that hook `speedbar-before-visiting-file-hook'."
  (select-window (previous-window)))

(defun sr-speedbar-before-visiting-tag-hook ()
  "Function that hook `speedbar-before-visiting-tag-hook'."
  (select-window (previous-window)))

(defun sr-speedbar-visiting-file-hook ()
  "Function that hook `speedbar-visiting-file-hook'."
  (select-window (previous-window)))

(defun sr-speedbar-visiting-tag-hook ()
  "Function that hook `speedbar-visiting-tag-hook'."
  (select-window (previous-window)))

(defun sr-speedbar-kill-buffer-hook ()
  "Function that hook `kill-buffer-hook'."
  (when (eq (current-buffer) speedbar-buffer)
    (setq speedbar-frame nil
          dframe-attached-frame nil
          speedbar-buffer nil)
    (speedbar-set-timer nil)
    (remove-hook 'speedbar-before-visiting-file-hook 'sr-speedbar-before-visiting-file-hook)
    (remove-hook 'speedbar-before-visiting-tag-hook 'sr-speedbar-before-visiting-tag-hook)
    (remove-hook 'speedbar-visiting-file-hook 'sr-speedbar-visiting-file-hook)
    (remove-hook 'speedbar-visiting-tag-hook 'sr-speedbar-visiting-tag-hook)
    ))

(defun sr-speedbar-refresh ()
  "Refresh the context of speedbar."
  (let ((name (if (eq major-mode 'dired-mode) ;get current buffer name
                  (dired-get-filename)
                (or (buffer-file-name) "")))
        current-refresh-directory)
    (setq current-refresh-directory (file-name-directory name)) ;get current directory
    (if (and (not (equal current-refresh-directory sr-speedbar-last-refresh-dictionary)) ;if directory is not change
             (not (equal sr-speedbar-buffer-name (buffer-name (window-buffer))))) ;and is not in speedbar buffer
        (progn
          (setq sr-speedbar-last-refresh-dictionary current-refresh-directory)
          (speedbar-refresh)
          ))))

(add-hook 'speedbar-timer-hook 'sr-speedbar-refresh) ;automatic update context of speedbar,

(defun current-window-take-width (&optional window)
  "Return the width that WINDOW take up,
Not the value of `window-width', it return usable columns available for WINDOW.
If WINDOW is nil, get current window."
  (let ((edges (window-edges window)))
    (- (nth 2 edges) (nth 0 edges))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Advices ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice delete-other-windows (around sr-speedbar-advice activate)
  "This advice to make `sr-speedbar' window can't deleted by
command `delete-other-windows'."
  (let (sr-speedbar-active-p)
    (if (window-live-p sr-speedbar-window)
        (setq sr-speedbar-active-p t))
    ad-do-it
    (if sr-speedbar-active-p
        (sr-speedbar-open)
      )))

(defadvice delete-window (around sr-speedbar-advice activate)
  "This advice to make `sr-speedbar' window can't deleted by
command `delete-window'."
  (let (sr-speedbar-active-p)
    (if (window-live-p sr-speedbar-window)
        (setq sr-speedbar-active-p t))
    ad-do-it
    (if sr-speedbar-active-p
        (progn
          (sr-speedbar-open)
          (message "Please use `sr-speedbar-toggle' to kill me!")))))

(provide 'sr-speedbar)

;;; sr-speedbar.el ends here
