;;; yeetube-ol.el --- Yeetube org-link integration.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Steven Allen

;; Author: Steven Allen <steven@stebalien.com>
;; Keywords: extensions youtube videos org
;; URL: https://git.thanosapollo.org/yeetube

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

;; This package is an org-link integration for yeetube.

;;; Code:

(require 'yeetube)
(require 'ol)
(require 'ox)

(defsubst yeetube-ol--store-link (type)
  "Create an Org link to current Yeetube item of TYPE.
TYPE must be either `video' or `playlist'.
This function does nothing if the current major mode is not `yeetube-mode'
and/or the item at point is not of type TYPE."
  (when (derived-mode-p 'yeetube-mode)
    (let* ((id (or (tabulated-list-get-id)
                   (save-excursion (end-of-line) (tabulated-list-get-id))))
           (entry (cadr (assoc id yeetube-content)))
           (title (aref entry (if yeetube-display-thumbnails-p 1 0))))
      (when (eq (aref entry (1- (length entry))) type)
        (org-link-store-props :type (format "yt-%S" type)
                              :link (format "yt-%S:%s" type id)
                              :description title)))))

(defun yeetube-ol--export-link-with-backend (backend info link &optional desc)
  "Helper function for exporting links with another backend.
Uses BACKEND to export a link with path LINK and description DESC.
The INFO is passed along to the export backend."
  (org-export-with-backend
   backend
   (car (org-element-parse-secondary-string
         (org-link-make-string link desc) '(link)))
   desc info))

;;;###autoload
(defun yeetube-ol--store-video-link (&optional _interactive)
  "Store an Org link to the current Yeetube video."
  (yeetube-ol--store-link 'video))

;;;###autoload
(defun yeetube-ol--export-video-link (path desc backend info)
  "Export a Yeetube video link for Org export.
PATH is the video ID, DESC is the link description.
BACKEND is the export backend being used and INFO is the export plist."
  (yeetube-ol--export-link-with-backend backend info (concat yeetube-video-url path) desc))

;;;###autoload
(defun yeetube-ol--follow-video-link (path _arg)
  "Open a Yeetube video link specified by PATH."
  (funcall yeetube-play-function (concat yeetube-video-url path)))

;;;###autoload
(defun yeetube-ol--store-playlist-link (&optional _interactive)
  "Store an Org link to the current Yeetube playlist."
  (yeetube-ol--store-link 'playlist))

;;;###autoload
(defun yeetube-ol--export-playlist-link (path desc backend info)
  "Export a Yeetube playlist link for Org export.
PATH is the playlist ID, DESC is the link description.
BACKEND is the export backend being used and INFO is the export plist."
  (yeetube-ol--export-link-with-backend backend info (concat yeetube-playlist-url path) desc))

;;;###autoload
(defun yeetube-ol--follow-playlist-link (path _arg)
  "Open a Yeetube playlist link specified by PATH."
  (pop-to-buffer-same-window "*yeetube*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Loading..." 'face 'bold-italic)))
  (yeetube-display-content-from-url (concat yeetube-playlist-url path)))

;;;###autoload
(with-eval-after-load 'ol
  (org-link-set-parameters
   "yt-video"
   :store #'yeetube-ol--store-video-link
   :export #'yeetube-ol--export-video-link
   :follow #'yeetube-ol--follow-video-link)
  (org-link-set-parameters
   "yt-playlist"
   :store #'yeetube-ol--store-playlist-link
   :export #'yeetube-ol--export-playlist-link
   :follow #'yeetube-ol--follow-playlist-link))

(provide 'yeetube-ol)
;;; yeetube-ol.el ends here
