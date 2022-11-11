
(autoload 'tea-time "tea-time" nil t)
(autoload 'tomatinho "tomatinho" nil t)
(autoload 'speed-type-region "speed-type" nil t)
(global-set-key (kbd "C-f t") 'speed-type-region)


(setq esup-insignificant-time 0.0001)
(autoload 'esup "esup" nil t)


;; remember to run
;; >> cd ~/tools/bin; ln -s /usr/bin/google-chrome chromium
(setq browse-url-browser-function 'browse-url-chromium)
(eval-after-load "eww"
  '(progn
     (setq eww-download-directory temporary-file-directory
           shr-inhibit-images t
           shr-use-fonts nil)
     (define-key eww-mode-map (kbd ",") 'eww-back-url)
     (define-key eww-mode-map (kbd ".") 'eww-forward-url)
     (define-key eww-mode-map (kbd "/") 'eww-follow-link)
     (define-key eww-mode-map (kbd "M-/") 'eww-open-in-new-buffer)
     (define-key eww-mode-map (kbd "C-/")
       #'(lambda ()
           (interactive)
           (let ((url (get-text-property (point) 'shr-url)))
             (cond
              ((not url) (message "No link under point"))
              (t (funcall browse-url-secondary-browser-function url))))))
     ))
