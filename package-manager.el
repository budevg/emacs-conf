(require 'cl) ; a rare necessary use of REQUIRE
(defvar *emacs-load-start* (current-time))

;; rest of your .emacs goes here

(defun package-manager-time-delta (old-time)
  (let* ((new-time (current-time))
         (old-time-in-msecs (+ (* (second old-time) 1000000)
                               (third old-time)))
         (new-time-in-msecs (+ (* (second new-time) 1000000)
                               (third new-time))))
;;    (message "start at:%s end at:%s" old-time new-time)
    (- new-time-in-msecs old-time-in-msecs)))

(defun package-manager-get-load-time ()
  (format "EMACS LOADED at %f seconds"
          (/ (package-manager-time-delta *emacs-load-start*) (float 1000000))))

(defun package-manager-show-load-time ()
  (message (package-manager-get-load-time)))

(defun package-manager-timer (start)
  (if start
      (setq package-manager-timer-start (current-time))
    (package-manager-time-delta package-manager-timer-start)))
