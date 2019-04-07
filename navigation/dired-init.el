
(autoload 'dired-subtree-cycle "dired-subtree" "" t)
(autoload 'dired-narrow "dired-narrow" "" t)

(eval-after-load "dired"
  '(progn
     (defun my-dired-init ()
       ;; make dired reuse the current buffer instead of opening new buffer
       ;; for each directory
       (autoload 'dired-single-buffer "dired-single" "" t)
       (autoload 'dired-single-buffer-mouse "dired-single" "" t)
       (autoload 'dired-single-magic-buffer "dired-single" "" t)
       (autoload 'dired-single-toggle-buffer-name "dired-single" "" t)
       (define-key dired-mode-map [tab] 'dired-subtree-cycle)
       (define-key dired-mode-map [(control n)] 'dired-narrow)
       (define-key dired-mode-map [return] 'dired-single-buffer)
       (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse)
       (define-key dired-mode-map "^"
         (function
          (lambda nil (interactive) (dired-single-buffer "..")))))

     (defun dired-visit-file-with-less ()
       (interactive)
       (let ((file-path (dired-get-file-for-visit)))
         (multi-term)
         (let ((term-proc (get-buffer-process (current-buffer))))
           (term-simple-send term-proc
                             (format "less -j4 '%s'" file-path)))))

     (define-key dired-mode-map [(meta return)] 'dired-visit-file-with-less)
     (defun dired-sort ()
       (interactive)
       (let ((sort-flags-table (make-hash-table))
             (sort-types-list '()))
         (dolist (entry '(("normal". "")
                          ("rtime". "t")
                          ("time" . "rt")
                          ("rsize" . "S")
                          ("size" . "rS")
                          ("rname" . "X")
                          ("name" . "rX")
                          ("rdir" . "U")
                          ("dir" . "rU")))
           (puthash (car entry) (cdr entry) sort-flags-table)
           (setq sort-types-list (cons (car entry) sort-types-list)))

         (let ((sort-flag (ido-completing-read
                           "Dired Sort: "
                           sort-types-list)))
           (setq dired-actual-switches (concat "-al" (gethash sort-flag sort-flags-table "")))
           (revert-buffer))))
     (define-key dired-mode-map "s" 'dired-sort)

     (if (boundp 'dired-mode-map)
         (my-dired-init)
       (add-hook 'dired-load-hook 'my-dired-init))

     (define-key dired-mode-map [(control x) (control q)] 'wdired-change-to-wdired-mode)
     (require 'dired-x)
     (setq dired-omit-extensions (append dired-omit-extensions
                                         '(".ko" ".ko.cmd" ".mod.c" ".mod.o.cmd" ".o.cmd" ".d") ))
     (define-key dired-mode-map [(meta o)] 'dired-omit-mode)
     (defun sudired ()
       (interactive)
       (require 'tramp)
       (let ((dir (expand-file-name default-directory)))
         (if (string-match "^/sudo:" dir)
             (user-error "Already in sudo")
           (dired (concat "/sudo::" dir)))))
     (define-key dired-mode-map "!" 'sudired)))
