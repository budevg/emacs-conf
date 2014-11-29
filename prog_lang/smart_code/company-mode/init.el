(defun load-company-mode ()
  (setq company-backends
        '(company-bbdb
          company-nxml
          company-css
          company-eclim
          company-semantic
          ;;company-clang
          company-xcode
          ;;company-ropemacs
          company-cmake
          company-capf
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-oddmuse
          company-files
          company-dabbrev
          ))
  (require 'company)
  (global-company-mode t)
  )

(eval-after-load "company"
  '(progn
     (custom-set-faces
      '(company-preview
        ((t (:foreground "darkgray" :underline t))))
      '(company-preview-common
        ((t (:inherit company-preview))))
      '(company-tooltip
        ((t (:background "lightgray" :foreground "black"))))
      '(company-tooltip-selection
        ((t (:background "steelblue" :foreground "white"))))
      '(company-tooltip-common
        ((((type x)) (:inherit company-tooltip :weight bold))
         (t (:inherit company-tooltip))))
      '(company-tooltip-common-selection
        ((((type x)) (:inherit company-tooltip-selection :weight bold))
         (t (:inherit company-tooltip-selection)))))
     (define-key company-active-map "\C-q" 'company-search-candidates)
     (define-key company-active-map "\C-e" 'company-filter-candidates)
     ))

(run-with-idle-timer 4 nil 'load-company-mode)
