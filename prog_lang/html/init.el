(defun replace-html-chars-region (start end)
  "Replace  “&lt;” to “<” and other chars in HTML.
This works on the current region."
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "&amp;" nil t) (replace-match "&" nil t))
    (goto-char (point-min))
    (while (search-forward "&lt;" nil t) (replace-match "<" nil t))
    (goto-char (point-min))
    (while (search-forward "&gt;" nil t) (replace-match ">" nil t))
    ))


(eval-after-load "nxml-mode"
  '(progn
     (defun nxml-forward-element()
       (let ((nxml-sexp-element-flag))
         (setq nxml-sexp-element-flag (not (looking-at "<!--")))
         (unless (looking-at outline-regexp)
           (condition-case nil
               (nxml-forward-balanced-item 1)
             (error nil)))))

     (add-to-list 'hs-special-modes-alist
                  '(nxml-mode
                    "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                    ""
                    "<!--" ;; won't work on its own; uses syntax table
                    (lambda (arg) (nxml-forward-element))
                    nil))

     (add-hook 'nxml-mode-hook
               (lambda ()
                 (make-local-variable 'outline-regexp)
                 (setq outline-regexp "\\s *<\\([h][1-6]\\|html\\|body\\|head\\)\\b")))
     ))
