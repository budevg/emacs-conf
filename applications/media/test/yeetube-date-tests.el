;;; yeetube-date-tests.el --- Date sorting regression tests -*- lexical-binding: t; -*-

;; Run from emacs-conf:
;; emacs -Q --batch -L applications/media -l applications/media/test/yeetube-date-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'yeetube-ui)
(require 'yeetube-scraper)

;; Defaults normally supplied by yeetube.el; keep these tests UI-only.
(defvar yeetube-display-thumbnails-p nil)
(defvar yeetube-thumbnail-size '(100 . 100))
(defvar yeetube-default-sort-column nil)
(defvar yeetube-default-sort-ascending nil)
(defvar yeetube-content nil)

(ert-deftest yeetube-date-missing-and-unknown ()
  "Missing dates and unknown labels do not abort sorting."
  (dolist (date '(nil "" "   " "LIVE" "5 fortnights ago"))
    (should (= 0 (yeetube-ui--parse-relative-date date)))))

(ert-deftest yeetube-date-relative-labels ()
  "Parse relative ages, including labels used for streams and premieres."
  (dolist (case '(("30 seconds ago" 30)
                  ("1 minute ago" 60)
                  ("2 hours ago" 7200)
                  ("Streamed 2 days ago" 172800)
                  ("Premiered 1 week ago" 604800)
                  ("  Premiered   2 months ago  " 5184000)
                  ("30s ago" 30)
                  ("2m ago" 120)
                  ("10h ago" 36000)
                  ("1d ago" 86400)
                  ("2w ago" 1209600)
                  ("8mo ago" 20736000)
                  ("10y ago" 315360000)
                  ("1 year ago" 31536000)))
    (should (= (- 1800000000 (cadr case))
               (yeetube-ui--parse-relative-date (car case) 1800000000)))))

(ert-deftest yeetube-date-mixed-chronology ()
  "Absolute timestamps and relative labels share chronological order."
  (let ((now 1800000000))
    (should (< (yeetube-ui--parse-relative-date "2020-01-01T00:00:00+00:00" now)
               (yeetube-ui--parse-relative-date "1 day ago" now)))
    (should (< (yeetube-ui--parse-relative-date "2 days ago" now)
               (yeetube-ui--parse-relative-date "1 day ago" now)))))

(ert-deftest yeetube-date-equal-labels ()
  "Equal ages compare equal even while the clock advances."
  (let ((yeetube-display-thumbnails-p nil)
        (clock 1800000000)
        (a '("a" ["A" "1" "1:00" "1 day ago" "C"]))
        (b '("b" ["B" "1" "1:00" "1 day ago" "C"])))
    (cl-letf (((symbol-function 'float-time)
               (lambda (&rest _) (cl-incf clock))))
      (should-not (yeetube-ui--sort-date a b))
      (should-not (yeetube-ui--sort-date b a)))))

(ert-deftest yeetube-date-sort-command ()
  "Date sorts on render and native sorting reverses either layout."
  (dolist (thumbnails '(nil t))
    (let ((yeetube-display-thumbnails-p thumbnails)
          (yeetube-thumbnail-size '(100 . 100))
          (yeetube-default-sort-column "Date")
          (yeetube-default-sort-ascending nil)
          (yeetube-content nil))
      (with-temp-buffer
        (tabulated-list-mode)
        (yeetube-ui-render
         '((:id "new" :title "New" :date "10h ago")
           (:id "unknown" :title "Unknown" :date "")
           (:id "old" :title "Old" :date "8mo ago")))
        (should (equal (mapcar #'car tabulated-list-entries)
                       '("new" "old" "unknown")))
        (goto-char (point-min))
        (search-forward "10h ago")
        (backward-char)
        (call-interactively #'tabulated-list-sort)
        (should (equal (mapcar #'car tabulated-list-entries)
                       '("unknown" "old" "new")))
        (goto-char (point-min))
        (search-forward "10h ago")
        (backward-char)
        (call-interactively #'tabulated-list-sort)
        (should (equal (mapcar #'car tabulated-list-entries)
                       '("new" "old" "unknown")))))))

(ert-deftest yeetube-column-cycle ()
  "TAB and S-TAB cycle through columns within the same row."
  (dolist (thumbnails '(nil t))
    (let ((yeetube-display-thumbnails-p thumbnails)
          (yeetube-default-sort-column nil))
      (with-temp-buffer
        (tabulated-list-mode)
        (yeetube-ui-render '((:id "a" :title "A" :date "1d ago" :channel "C")))
        (dotimes (i (length tabulated-list-format))
          (should (equal (get-text-property (point) 'tabulated-list-column-name)
                         (car (aref tabulated-list-format i))))
          (yeetube-ui-cycle-column))
        (should (equal (get-text-property (point) 'tabulated-list-column-name)
                       (car (aref tabulated-list-format 0))))
        (dotimes (i (length tabulated-list-format))
          (yeetube-ui-cycle-column t)
          (should (equal (get-text-property (point) 'tabulated-list-column-name)
                         (car (aref tabulated-list-format
                                    (- (length tabulated-list-format) i 1))))))))))

(ert-deftest yeetube-channel-metadata ()
  "Channel lockups carry abbreviated views and page-level channel data."
  (let* ((renderer '((contentId . "a")
                     (metadata (lockupMetadataViewModel
                                (metadata (contentMetadataViewModel
                                           (metadataRows
                                            ((metadataParts
                                              ((text (content . "17K"))
                                               (accessibilityLabel . "17 thousand views")))))))))))
         (item (yeetube-scraper--extract-lockup-video renderer))
         (page '((metadata (channelMetadataRenderer
                            (title . "comma") (externalId . "UC123")))
                 (contents (twoColumnBrowseResultsRenderer (tabs . nil))))))
    (should (equal (plist-get item :views) "17K"))
    (should (equal (yeetube-ui--format-views (plist-get item :views)) "17K"))
    (let ((yeetube-display-thumbnails-p nil))
      (should (yeetube-ui--sort-views '("a" ["A" "17K"])
                                     '("b" ["B" "2M"])))
      (should (yeetube-ui--sort-views '("a" ["A" ""])
                                     '("b" ["B" "17K"]))))
    (should (equal (plist-get (yeetube-scraper--parse-channel page) :channel)
                   "comma"))
    (should (equal (plist-get (yeetube-scraper--parse-channel page) :channel-id)
                   "UC123"))))

;;; yeetube-date-tests.el ends here
