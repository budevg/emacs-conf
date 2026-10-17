;;; yeetube-thumbnail-tests.el --- Thumbnail URL regression tests -*- lexical-binding: t; -*-

;; Run from emacs-conf:
;; emacs -Q --batch -L applications/media -l applications/media/test/yeetube-thumbnail-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'yeetube-scraper)

(ert-deftest yeetube-thumbnail-preserves-custom-url ()
  "Custom thumbnails retain their filename and signed query parameters."
  (let ((url "https://i.ytimg.com/vi/3XI90A3ENSg/hq720_custom_1.jpg?sqp=signed&rs=signature"))
    (should (equal (yeetube-scraper--thumbnail-url
                    "3XI90A3ENSg" (list (list (cons 'url url))))
                   url))
    ;; Both search videoRenderer and channel lockupViewModel use the helper.
    (dolist (renderer
             `(((videoRenderer
                 (videoId . "3XI90A3ENSg")
                 (thumbnail (thumbnails ((url . ,url))))))
               ((lockupViewModel
                 (contentId . "3XI90A3ENSg")
                 (contentType . "LOCKUP_CONTENT_TYPE_VIDEO")
                 (contentImage (thumbnailViewModel
                                (image (sources ((url . ,url))))))))))
      (should (equal (plist-get (yeetube-scraper--dispatch-item renderer)
                                :thumbnail-url)
                     url)))))

(ert-deftest yeetube-thumbnail-standard-small-variant ()
  "Only standard hq720.jpg filenames are replaced by the small variant."
  (dolist (suffix '("" "?sqp=signed&rs=signature"))
    (should (equal (yeetube-scraper--thumbnail-url
                    "video" `(((url . ,(concat "https://i.ytimg.com/vi/video/hq720.jpg"
                                               suffix)))))
                   "https://i.ytimg.com/vi/video/default.jpg"))))

(ert-deftest yeetube-thumbnail-preserves-other-urls ()
  "Other supplied thumbnail URLs are not stripped or rewritten."
  (dolist (url '("https://i.ytimg.com/vi/video/hqdefault.jpg"
                 "https://i.ytimg.com/vi/video/hqdefault.jpg?sqp=signed"
                 "https://i.ytimg.com/vi_webp/video/hq720.webp?sqp=signed"))
    (should (equal (yeetube-scraper--thumbnail-url
                    "video" (list (list (cons 'url url))))
                   url))))

(ert-deftest yeetube-thumbnail-missing-url ()
  "Missing thumbnail data uses the predictable default JPEG URL."
  (dolist (thumbnails '(nil (nil)))
    (should (equal (yeetube-scraper--thumbnail-url "video" thumbnails)
                   "https://i.ytimg.com/vi/video/default.jpg"))))

;;; yeetube-thumbnail-tests.el ends here
