;;; yeetube-download.el --- YeeTube download support  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://thanosapollo.org/projects/yeetube/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Download support for YeeTube using yt-dlp.

;;; Code:

(require 'cl-lib)

;; Forward declarations for variables defined in yeetube.el
(defvar yeetube-ytdlp-program)
(defvar yeetube-torsocks-program)
(defvar yeetube-enable-tor)
(defvar yeetube-download-directory)
(defvar yeetube-download-audio-format)

(defun yeetube-download--ytdlp (url &optional name audio-format)
  "Download URL using yt-dlp.

Optional values:
 NAME for custom file name.
 AUDIO-FORMAT to extract and keep contents as specified audio-format only."
  (unless yeetube-ytdlp-program
    (error "Executable for yt-dlp not found. Please set yeetube-ytdlp-program"))
  (let* ((tor-command (when yeetube-enable-tor yeetube-torsocks-program))
         (name-command (when name (format "-o %s" (shell-quote-argument name))))
         (format-command (when audio-format
			   (format "--extract-audio --audio-format %s"
				   (shell-quote-argument audio-format))))
         (command (mapconcat 'identity (delq nil
					     (list tor-command
						   yeetube-ytdlp-program
						   (shell-quote-argument url)
						   name-command format-command))
			     " ")))
    (call-process-shell-command command nil 0)))

;;;###autoload
(defun yeetube-download-change-directory ()
  "Change download directory."
  (interactive)
  (setf yeetube-download-directory
        (read-directory-name "Select a directory: ")))

;;;###autoload
(defun yeetube-download-change-audio-format (audio-format)
  "Change download format to AUDIO-FORMAT."
  (interactive "sSpecify Audio Format(no for nil): ")
  (setf yeetube-download-audio-format (and (not (equal audio-format "no"))
                                           audio-format)))

;; TODO: Add option to use ffmpeg
;;;###autoload
(defun yeetube-download-videos ()
  "Bulk download videos using yt-dlp.
This command is not meant to be used in the *Yeetube Search* buffer.

Usage Example:
Open a Dired buffer and navigate where you want to download your
videos, then run this command interactively.  You can leave the name
prompt blank to keep the default name."
  (interactive)
  (let ((download-counter 1))
    (cl-loop
     for url = (read-string "Enter URL (q to quit): ")
     until (string= url "q")
     do (let ((name (read-string (format "Custom name (download counter: %d) "
					 download-counter))))
          (yeetube-download--ytdlp url name yeetube-download-audio-format)
          (cl-incf download-counter)))))

(provide 'yeetube-download)
;;; yeetube-download.el ends here
