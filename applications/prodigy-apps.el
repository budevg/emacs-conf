(autoload 'prodigy "prodigy" nil t)
(global-set-key [(meta p)] 'prodigy)
(eval-after-load "prodigy"
  '(progn
     ;;(prodigy-define-service
     ;;  :name ""
     ;;  :command ""
     ;;  :args '("" "")
     ;;  :cwd ""
     ;;  :tags '()
     ;;  :init ""
     ;;  :init-async ""
     ;;  :stop-signal 'sigkill
     ;;  :path ""
     ;;  :env ""
     ;;  :url ""
     ;;  :kill-process-buffer-on-stop t
     ;;  :on-output "")
     ))
