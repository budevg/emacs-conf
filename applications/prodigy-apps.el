(autoload 'prodigy "prodigy" nil t)
(global-set-key [(meta p)] 'prodigy)


(eval-after-load "prodigy"
  '(progn
     (defun prodigy-getp (args p)
       (let ((service (plist-get args :service)))
         (prodigy-service-or-first-tag-with service p)))

     (prodigy-define-tag
      :name 'qemu
      :command "kvm"
      :boot-disk "/dev/zero"
      :nic-device "virtio"
      :disk-device "virtio"
      :mem "1G"
      :args
      (lambda (&rest args)
        (list "-snapshot"
              "-m" (prodigy-getp args :mem)
              "-net" "user,hostfwd=tcp::5555-:22"
              "-net" (format "nic,model=%s" (prodigy-getp args :nic-device))
              "-drive" (format "if=%s,file=%s"
                               (prodigy-getp args :disk-device)
                               (prodigy-getp args :boot-disk))
              ))
      :cwd "~/scratch"
      :kill-process-buffer-on-stop t
      )

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
