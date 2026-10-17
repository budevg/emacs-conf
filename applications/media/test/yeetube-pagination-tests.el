;;; yeetube-pagination-tests.el --- Pagination regression tests -*- lexical-binding: t; -*-

;; Run from emacs-conf:
;; emacs -Q --batch -L applications/media -l applications/media/test/yeetube-pagination-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'yeetube-scraper)

(ert-deftest yeetube-pagination-channel-response ()
  "Channel responses yield grid videos and the next continuation."
  (let* ((video '((richItemRenderer
                  (content (lockupViewModel
                            (contentId . "video-2")
                            (contentType . "LOCKUP_CONTENT_TYPE_VIDEO")
                            (metadata (lockupMetadataViewModel
                                       (title (content . "Next video")))))))))
         (continuation '((continuationItemRenderer
                          (continuationEndpoint
                           (commandMetadata
                            (webCommandMetadata (apiUrl . "/youtubei/v1/browse")))
                           (continuationCommand (token . "next-page"))))))
         (action `((appendContinuationItemsAction
                    (continuationItems . ,(list video continuation))))))
    (dolist (root '(onResponseReceivedActions
                    onResponseReceivedCommands
                    onResponseReceivedEndpoints))
      ;; Unrelated commands can precede the append action.
      (dolist (prefix '(nil (((unrelatedCommand (value . t))))))
        (let* ((response (list (cons root (append prefix (list action)))))
               (result (yeetube-scraper-parse-continuation-response response))
               (items (plist-get result :items)))
          (should (= (length items) 1))
          (should (equal (plist-get (car items) :id) "video-2"))
          (should (equal (plist-get (car items) :title) "Next video"))
          (should (equal (plist-get result :continuation)
                         '(:token "next-page" :url "/youtubei/v1/browse"))))))))

(ert-deftest yeetube-pagination-search-response ()
  "Search continuations still yield section videos without a next token."
  (let* ((response '((onResponseReceivedCommands
                     ((appendContinuationItemsAction
                       (continuationItems
                        ((itemSectionRenderer
                          (contents
                           ((videoRenderer
                             (videoId . "search-2")
                             (title (runs ((text . "Search result")))))))))))))))
         (result (yeetube-scraper-parse-continuation-response response)))
    (should (equal (plist-get (car (plist-get result :items)) :id) "search-2"))
    (should-not (plist-get result :continuation))))

(ert-deftest yeetube-pagination-empty-response ()
  "Responses without an append action yield no items or continuation."
  (should (equal (yeetube-scraper-parse-continuation-response nil)
                 '(:items nil :continuation nil))))

;;; yeetube-pagination-tests.el ends here
