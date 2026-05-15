;;; yeetube-scraper.el --- Pure YouTube JSON parsing for yeetube  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Thanos Apollo

;; Author: Thanos Apollo <public@thanosapollo.org>
;; Keywords: extensions youtube videos
;; URL: https://thanosapollo.org/projects/yeetube/

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Pure data extraction from YouTube's ytInitialData JSON.
;; All private functions are side-effect free.
;; The public entry point `yeetube-scraper-parse' reads from the
;; current buffer but restores point via `save-excursion'.
;; Input: parsed JSON (alists).  Output: plists.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun yeetube-scraper--extract-video (renderer)
  "Extract a video plist from a videoRenderer RENDERER alist."
  (let* ((title-runs (alist-get 'runs (alist-get 'title renderer)))
         (title (alist-get 'text (car title-runs)))
         (views (alist-get 'simpleText (alist-get 'viewCountText renderer)))
         (duration (alist-get 'simpleText (alist-get 'lengthText renderer)))
         (date-raw (alist-get 'simpleText (alist-get 'publishedTimeText renderer)))
         (date (when date-raw (string-replace "Streamed " "" date-raw)))
         (byline-runs (alist-get 'runs (alist-get 'longBylineText renderer)))
         (channel (alist-get 'text (car byline-runs)))
         (browse-endpoint (alist-get 'browseEndpoint
				     (alist-get 'navigationEndpoint (car byline-runs))))
         (channel-id (alist-get 'canonicalBaseUrl browse-endpoint))
         (browse-id (alist-get 'browseId browse-endpoint))
         (thumbs (alist-get 'thumbnails (alist-get 'thumbnail renderer)))
         (thumb-url (yeetube-scraper--thumbnail-url
                     (alist-get 'videoId renderer) thumbs)))
    (list :id (alist-get 'videoId renderer)
          :title (or title "")
          :views (or views "")
          :duration (or duration "")
          :date (or date "")
          :channel (or channel "")
          :channel-id (or channel-id "")
          :browse-id (or browse-id "")
          :thumbnail-url (or thumb-url "")
          :type 'video)))

(defun yeetube-scraper--thumbnail-url (video-id thumbnails)
  "Build a small thumbnail URL for VIDEO-ID from THUMBNAILS list.
Falls back to the predictable default.jpg URL pattern."
  (or (and-let* ((url (alist-get 'url (car thumbnails)))
                 (qmark (string-search "?" url))
                 (base (substring url 0 qmark)))
        ;; Use the smaller "default" variant instead of hq720
        (string-replace "hq720" "default" base))
      (format "https://i.ytimg.com/vi/%s/default.jpg" video-id)))

;;; Playlist extraction (lockupViewModel)

(defun yeetube-scraper--metadata-part-texts (metadata-rows)
  "Return text contents from METADATA-ROWS."
  (cl-loop for row in metadata-rows
           append (cl-loop for part in (alist-get 'metadataParts row)
                           for text = (alist-get 'content
                                                 (alist-get 'text part))
                           when text collect text)))

(defun yeetube-scraper--row-text (row)
  "Extract the content string from a metadata ROW."
  (alist-get 'content
	     (alist-get 'text
			(car (alist-get 'metadataParts row)))))

(defun yeetube-scraper--playlist-channel (metadata-rows)
  "Extract channel name from playlist METADATA-ROWS."
  (yeetube-scraper--row-text (car metadata-rows)))

(defun yeetube-scraper--playlist-video-count (metadata-rows)
  "Extract video count string from playlist METADATA-ROWS."
  (when-let* ((found (cl-find-if
                      (lambda (row)
                        (and-let* ((text (yeetube-scraper--row-text row)))
                          (string-match-p "video" text)))
                      metadata-rows)))
    (yeetube-scraper--row-text found)))

(defun yeetube-scraper--playlist-thumbnail (renderer)
  "Extract thumbnail URL from playlist RENDERER."
  (let* ((image (alist-get 'contentImage renderer))
         (collection (alist-get 'collectionThumbnailViewModel image))
         (primary (alist-get 'primaryThumbnail collection))
         (thumb-vm (alist-get 'thumbnailViewModel primary))
         (sources (alist-get 'sources (alist-get 'image thumb-vm))))
    (alist-get 'url (car sources))))

(defun yeetube-scraper--lockup-thumbnail (renderer)
  "Extract thumbnail URL from lockupViewModel RENDERER."
  (let* ((image (alist-get 'contentImage renderer))
         (thumb-vm (alist-get 'thumbnailViewModel image))
         (sources (alist-get 'sources (alist-get 'image thumb-vm))))
    (yeetube-scraper--thumbnail-url
     (alist-get 'contentId renderer)
     sources)))

(defun yeetube-scraper--lockup-duration (renderer)
  "Extract video duration from lockupViewModel RENDERER."
  (let* ((image (alist-get 'contentImage renderer))
         (thumb-vm (alist-get 'thumbnailViewModel image))
         (overlays (alist-get 'overlays thumb-vm)))
    (cl-loop for overlay in overlays
             for badges = (alist-get
                           'badges
                           (alist-get 'thumbnailBottomOverlayViewModel overlay))
             thereis (cl-loop for badge in badges
                              for text = (alist-get
                                          'text
                                          (alist-get 'thumbnailBadgeViewModel badge))
                              when text return text))))

(defun yeetube-scraper--extract-lockup-video (renderer)
  "Extract a video plist from a lockupViewModel RENDERER alist."
  (let* ((meta (alist-get 'lockupMetadataViewModel
			  (alist-get 'metadata renderer)))
         (title (alist-get 'content (alist-get 'title meta)))
         (rows (alist-get 'metadataRows
			  (alist-get 'contentMetadataViewModel
				     (alist-get 'metadata meta))))
         (texts (yeetube-scraper--metadata-part-texts rows))
         (views (cl-find-if
                 (lambda (text) (string-match-p "\\bviews?\\b" text))
                 texts))
         (date-raw (cl-find-if
                    (lambda (text)
                      (string-match-p "\\(ago\\|Streamed\\|Premiered\\)" text))
                    texts))
         (date (when date-raw (string-replace "Streamed " "" date-raw)))
         (thumb-url (yeetube-scraper--lockup-thumbnail renderer)))
    (list :id (alist-get 'contentId renderer)
          :title (or title "")
          :views (or views "")
          :duration (or (yeetube-scraper--lockup-duration renderer) "")
          :date (or date "")
          :channel ""
          :channel-id ""
          :browse-id ""
          :thumbnail-url (or thumb-url "")
          :type 'video)))

(defun yeetube-scraper--extract-playlist (renderer)
  "Extract a playlist plist from a lockupViewModel RENDERER alist."
  (let* ((meta (alist-get 'lockupMetadataViewModel
			  (alist-get 'metadata renderer)))
         (title (alist-get 'content (alist-get 'title meta)))
         (rows (alist-get 'metadataRows
			  (alist-get 'contentMetadataViewModel
				     (alist-get 'metadata meta))))
         (channel (yeetube-scraper--playlist-channel rows))
         (video-count (yeetube-scraper--playlist-video-count rows))
         (thumb-url (yeetube-scraper--playlist-thumbnail renderer)))
    (list :id (alist-get 'contentId renderer)
          :title (or title "")
          :views ""
          :duration (or video-count "")
          :date ""
          :channel (or channel "")
          :channel-id ""
          :thumbnail-url (or thumb-url "")
          :type 'playlist)))

;;; Item dispatch

(defun yeetube-scraper--dispatch-item (item)
  "Dispatch ITEM to the appropriate extractor.  Return plist or nil."
  (cond
   ((alist-get 'richItemRenderer item)
    (yeetube-scraper--dispatch-item
     (alist-get 'content (alist-get 'richItemRenderer item))))
   ((alist-get 'videoRenderer item)
    (yeetube-scraper--extract-video (alist-get 'videoRenderer item)))
   ;; YouTube migrated playlists and channel videos to lockupViewModel.
   ((and-let* ((lockup (alist-get 'lockupViewModel item)))
      (equal (alist-get 'contentType lockup) "LOCKUP_CONTENT_TYPE_VIDEO"))
    (yeetube-scraper--extract-lockup-video (alist-get 'lockupViewModel item)))
   ((and-let* ((lockup (alist-get 'lockupViewModel item)))
      (equal (alist-get 'contentType lockup) "LOCKUP_CONTENT_TYPE_PLAYLIST"))
    (yeetube-scraper--extract-playlist (alist-get 'lockupViewModel item)))))

;;; Continuation token extraction

(defun yeetube-scraper--extract-continuation (sections)
  "Extract continuation token from SECTIONS.
Return plist (:token T :url U) or nil."
  (let ((cont-item
         (cl-find-if
          (lambda (s) (alist-get 'continuationItemRenderer s))
          sections)))
    (when cont-item
      (let* ((renderer (alist-get 'continuationItemRenderer cont-item))
             (endpoint (alist-get 'continuationEndpoint renderer))
             (token (alist-get 'token
			       (alist-get 'continuationCommand endpoint)))
             (url (alist-get 'apiUrl
			     (alist-get 'webCommandMetadata
					(alist-get 'commandMetadata endpoint)))))
        (when token
          (list :token token :url (or url "")))))))

;;; Section / grid item extraction

(defun yeetube-scraper--extract-section-items (sections)
  "Extract item plists from search result SECTIONS."
  (let* ((item-section
          (cl-find-if
           (lambda (s) (alist-get 'itemSectionRenderer s))
           sections))
         (items (alist-get 'contents
			   (alist-get 'itemSectionRenderer item-section))))
    (cl-loop for item in items
             for plist = (yeetube-scraper--dispatch-item item)
             when plist collect plist)))

(defun yeetube-scraper--extract-grid-items (grid-contents)
  "Extract item plists from channel page GRID-CONTENTS."
  (cl-loop for entry in grid-contents
           for plist = (yeetube-scraper--dispatch-item entry)
           when plist collect plist))

;;; Page-type parsers

(defun yeetube-scraper--parse-search (contents)
  "Parse search results from CONTENTS alist.
Return plist (:items ITEMS :continuation CONT)."
  (let* ((primary (alist-get 'primaryContents
			     (alist-get 'twoColumnSearchResultsRenderer contents)))
         (sections (alist-get 'contents
			      (alist-get 'sectionListRenderer primary)))
         (items (yeetube-scraper--extract-section-items sections))
         (continuation (yeetube-scraper--extract-continuation sections)))
    (list :items items :continuation continuation)))

(defun yeetube-scraper--parse-channel (contents)
  "Parse channel page from CONTENTS alist.
Return plist (:items ITEMS :continuation CONT)."
  (let* ((tabs (alist-get 'tabs
			  (alist-get 'twoColumnBrowseResultsRenderer contents)))
         (selected (cl-find-if
                    (lambda (tab)
                      (eq t (alist-get 'selected
				       (alist-get 'tabRenderer tab))))
                    tabs))
         (grid-contents
          (thread-last
            (alist-get 'tabRenderer selected)
            (alist-get 'content)
            (alist-get 'richGridRenderer)
            (alist-get 'contents)))
         (items (yeetube-scraper--extract-grid-items grid-contents))
         (continuation (yeetube-scraper--extract-continuation grid-contents)))
    (list :items items :continuation continuation)))

;;; Top-level buffer parser

(defun yeetube-scraper-parse ()
  "Parse ytInitialData from the current buffer.
Return plist (:items ITEM-PLISTS :continuation (:token T :url U)).
Point is restored after parsing."
  (save-excursion
    (goto-char (point-min))
    (search-forward "ytInitialData")
    (search-forward "=")
    (skip-chars-forward " \t\n")
    (let* ((json (json-parse-buffer :object-type 'alist :array-type 'list))
           (contents (alist-get 'contents json)))
      (cond
       ((alist-get 'twoColumnSearchResultsRenderer contents)
        (yeetube-scraper--parse-search contents))
       ((alist-get 'twoColumnBrowseResultsRenderer contents)
        (yeetube-scraper--parse-channel contents))
       (t (list :items nil :continuation nil))))))

;;; Continuation response parser

(defun yeetube-scraper-parse-continuation-response (json)
  "Parse a continuation/pagination JSON response.
Return (:items ... :continuation ...)."
  (let* ((commands (alist-get 'onResponseReceivedCommands json))
         (action (alist-get 'appendContinuationItemsAction (car commands)))
         (cont-items (alist-get 'continuationItems action)))
    (list :items (or (yeetube-scraper--extract-section-items cont-items)
                     (yeetube-scraper--extract-grid-items cont-items))
          :continuation (yeetube-scraper--extract-continuation cont-items))))

(provide 'yeetube-scraper)
;;; yeetube-scraper.el ends here
