
(autoload 'tea-time "tea-time" nil t)
(autoload 'tomatinho "tomatinho" nil t)
(global-set-key (kbd "C-f t") 'tomatinho)


(setq esup-insignificant-time 0.0001)
(autoload 'esup "esup" nil t)


;; remember to run
;; >> cd ~/tools/bin; ln -s /usr/bin/google-chrome chromium
(setq browse-url-browser-function 'browse-url-chromium)
