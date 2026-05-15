;;; yeetube-ui.el --- Display layer for yeetube  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://thanosapollo.org/projects/yeetube/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Display layer for yeetube.  Owns all display concerns: plist-to-vector
;; conversion, faces, sort functions, tabulated-list setup, and thumbnail
;; fetching/rendering.

;;; Code:

(require 'cl-lib)
(require 'image)
(require 'mm-decode)
(require 'tabulated-list)
(require 'url-queue)

;; Forward declarations -- variables from yeetube.el
(defvar yeetube-display-thumbnails-p)
(defvar yeetube-thumbnail-size)
(defvar yeetube-default-sort-column)
(defvar yeetube-default-sort-ascending)
(defvar yeetube-request-headers)
(defvar yeetube-enable-tor)
(defvar yeetube-content)

;; Forward declarations -- functions from yeetube.el
(declare-function yeetube-with-tor-socks "yeetube" (&rest body))


;;; Faces

(defgroup yeetube-faces nil
  "Faces used by yeetube."
  :group 'yeetube
  :tag "Yeetube Faces"
  :prefix 'yeetube-face)

(defface yeetube-face-header-query
  '((t :inherit font-lock-function-name-face))
  "Face used for the search query header."
  :group 'yeetube-faces)

(defface yeetube-face-duration
  '((t :inherit font-lock-string-face))
  "Face used for the video duration."
  :group 'yeetube-faces)

(defface yeetube-face-view-count
  '((t :inherit font-lock-keyword-face))
  "Face used for the video view count."
  :group 'yeetube-faces)

(defface yeetube-face-title
  '((t :inherit font-lock-variable-use-face))
  "Face used for video title."
  :group 'yeetube-faces)

(defface yeetube-face-channel
  '((t :inherit font-lock-function-call-face))
  "Face used for video channel name."
  :group 'yeetube-faces)

(defface yeetube-face-date
  '((t :inherit font-lock-doc-face))
  "Face used for published date."
  :group 'yeetube-faces)


;;; View count formatting

(defun yeetube-ui--format-views (views-string)
  "Format VIEWS-STRING by extracting digits and adding commas."
  (let ((digits (replace-regexp-in-string "[^0-9]" "" views-string)))
    (if (string-empty-p digits)
        ""
      (let ((len (length digits)))
        (string-join
         (nreverse
          (cl-loop for i from 0 below len by 3
                   collect (substring digits
                                      (max 0 (- len i 3))
                                      (- len i))))
         ",")))))


;;; Column index helper

(defun yeetube-ui--column-index (name)
  "Return the vector index for column NAME.
Accounts for the thumbnail column offset when thumbnails are enabled."
  (let ((offset (if yeetube-display-thumbnails-p 1 0))
        (base (pcase name
                ("title"    0)
                ("views"    1)
                ("duration" 2)
                ("date"     3)
                ("channel"  4)
                (_ (error "Unknown column: %s" name)))))
    (+ base offset)))


;;; Plist-to-vector conversion

(defun yeetube-ui--entry-to-row (entry)
  "Convert plist ENTRY to a tabulated-list row (ID VECTOR).
The vector layout depends on `yeetube-display-thumbnails-p'."
  (let* ((id (plist-get entry :id))
         (title (plist-get entry :title))
         (views (plist-get entry :views))
         (duration (plist-get entry :duration))
         (date (plist-get entry :date))
         (channel (plist-get entry :channel))
         (channel-id (plist-get entry :channel-id))
         (type (plist-get entry :type))
         (playlistp (eq type 'playlist))
         (title-str (propertize (if playlistp
                                    (concat "Playlist: " title)
                                  title)
                                'face 'yeetube-face-title))
         (views-str (propertize (yeetube-ui--format-views (or views ""))
                                'face 'yeetube-face-view-count))
         (duration-str (propertize (or duration "")
                                   'face 'yeetube-face-duration))
         (date-str (propertize (or date "") 'face 'yeetube-face-date))
         (channel-str (propertize (or channel "")
                                  'face 'yeetube-face-channel))
         (fields (list title-str views-str duration-str
                       date-str channel-str channel-id type)))
    (list id (if yeetube-display-thumbnails-p
                 (apply #'vector (format "[[%s.jpg]]" id) fields)
               (apply #'vector fields)))))


;;; Sort helpers

(defun yeetube-ui--duration-to-seconds (duration)
  "Convert DURATION string in HH:MM:SS format to total seconds."
  (pcase (mapcar #'string-to-number (split-string duration ":"))
    (`(,h ,m ,s) (+ (* h 3600) (* m 60) s))
    (`(,m ,s) (+ (* m 60) s))
    (`(,s) s)
    (_ 0)))

(defun yeetube-ui--parse-relative-date (date)
  "Convert relative DATE like \"2 days ago\" to comparable seconds."
  (let* ((split-date (split-string date " "))
         (value (string-to-number (nth 0 split-date)))
         (unit (nth 1 split-date))
         (seconds-per-unit
          (pcase unit
            ((or "second" "seconds") 1)
            ((or "minute" "minutes") 60)
            ((or "hour" "hours")     3600)
            ((or "day" "days")       86400)
            ((or "week" "weeks")     604800)
            ((or "month" "months")   2592000)
            ((or "year" "years")     31536000)
            (_ 0))))
    (* value seconds-per-unit)))

(defun yeetube-ui--sort-views (a b)
  "Sort entries A and B by view count."
  (let* ((idx (yeetube-ui--column-index "views"))
         (views-a (string-to-number
                   (replace-regexp-in-string "," "" (aref (cadr a) idx))))
         (views-b (string-to-number
                   (replace-regexp-in-string "," "" (aref (cadr b) idx)))))
    (< views-a views-b)))

(defun yeetube-ui--sort-duration (a b)
  "Sort entries A and B by duration."
  (let* ((idx (yeetube-ui--column-index "duration"))
         (dur-a (yeetube-ui--duration-to-seconds (aref (cadr a) idx)))
         (dur-b (yeetube-ui--duration-to-seconds (aref (cadr b) idx))))
    (< dur-a dur-b)))

(defun yeetube-ui--sort-date (a b)
  "Sort entries A and B by relative date."
  (let* ((idx (yeetube-ui--column-index "date"))
         (date-a (yeetube-ui--parse-relative-date (aref (cadr a) idx)))
         (date-b (yeetube-ui--parse-relative-date (aref (cadr b) idx))))
    (< date-a date-b)))


;;; Tabulated-list setup

(defun yeetube-ui--tabulated-list-format ()
  "Return the `tabulated-list-format' vector."
  (let* ((w (window-width))
         (thumb-col `("Thumbnail" ,(ceiling (/ (float (car yeetube-thumbnail-size))
                                               (frame-char-width)))
                      nil))
         (columns `[,@(when yeetube-display-thumbnails-p (list thumb-col))
                    ("Title"    ,(/ w 3) t)
                    ("Views"    ,(/ w 10) yeetube-ui--sort-views)
                    ("Duration" ,(/ w 10) yeetube-ui--sort-duration)
                    ("Date"     ,(/ w 8) yeetube-ui--sort-date)
                    ("Channel"  ,(/ w 8) t)]))
    columns))

(defun yeetube-ui-render (items)
  "Convert ITEMS (list of plists) to tabulated-list rows and display."
  (let ((rows (mapcar #'yeetube-ui--entry-to-row items)))
    (setf yeetube-content rows
          tabulated-list-format (yeetube-ui--tabulated-list-format)
          tabulated-list-entries yeetube-content
          tabulated-list-sort-key
          (when yeetube-default-sort-column
            (cons yeetube-default-sort-column
                  yeetube-default-sort-ascending)))
    (tabulated-list-init-header)
    (tabulated-list-print)))

(defun yeetube-ui-append (items)
  "Append ITEMS to the current yeetube buffer."
  (let ((new-rows (mapcar #'yeetube-ui--entry-to-row items)))
    (setf yeetube-content (append yeetube-content new-rows)
          tabulated-list-entries yeetube-content)
    (tabulated-list-print t)))


;;; Thumbnail fetching

(defun yeetube-ui--extract-image (status)
  "Extract thumbnail image from the current URL callback buffer.
Return the image sized per `yeetube-thumbnail-size', or nil on error.
STATUS is the URL retrieval callback status plist."
  (unless (plist-get status :error)
    (when-let* ((handle (mm-dissect-buffer t))
                (image (mm-get-image handle)))
      (setf (image-property image :max-width) (car yeetube-thumbnail-size)
            (image-property image :max-height) (cdr yeetube-thumbnail-size))
      image)))

(defun yeetube-ui--image-callback (status entry-id buffer)
  "Handle thumbnail image retrieval for ENTRY-ID.
STATUS is the URL retrieval callback status plist.
BUFFER is the yeetube display buffer name to update."
  (let* ((url-buffer (current-buffer))
         (image (yeetube-ui--extract-image status)))
    (kill-buffer url-buffer)
    (when image
      ;; Update the vector in yeetube-content
      (when-let* ((vec (cadr (assoc entry-id yeetube-content))))
        (aset vec 0 (propertize (aref vec 0) 'display image)))
      ;; Update the buffer text
      (when (get-buffer buffer)
        (with-current-buffer buffer
          (with-silent-modifications
            (save-excursion
              (goto-char (point-min))
              (when (search-forward (format "[[%s.jpg]]" entry-id) nil t)
                (add-text-properties (match-beginning 0) (match-end 0)
                                     `(display ,image))))))))))

(defun yeetube-ui-fetch-thumbnails (items buffer)
  "Fetch thumbnails for ITEMS and display them in BUFFER.
Each element in ITEMS is a plist with at least :id and :thumbnail-url."
  (when yeetube-display-thumbnails-p
    (let ((url-request-extra-headers yeetube-request-headers))
      (dolist (item items)
        (let ((url (plist-get item :thumbnail-url))
              (id (plist-get item :id)))
          (when (and url (not (string-empty-p url)))
            (if yeetube-enable-tor
                (yeetube-with-tor-socks
                 (url-queue-retrieve url #'yeetube-ui--image-callback
                                     (list id buffer)
                                     'silent 'inhibit-cookies))
              (url-queue-retrieve url #'yeetube-ui--image-callback
                                  (list id buffer)
                                  'silent 'inhibit-cookies))))))))

(provide 'yeetube-ui)
;;; yeetube-ui.el ends here
