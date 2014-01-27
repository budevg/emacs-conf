
(autoload 'fci-mode
  "fill-column-indicator" nil t)

(global-set-key [(control shift b)] 'fci-mode)

(autoload 'highlight-parentheses-mode
  "highlight-parentheses" "highlight-parentheses autoload" t)

(dolist (hook '(emacs-lisp-mode-hook
                html-mode-hook
                c-mode-hook
                c++-mode-hook
                cperl-mode-hook
                java-mode-hook))
  (add-hook hook
            (lambda ()
              (highlight-parentheses-mode t))))

(autoload 'highline-mode "highline" "highline autoload" t)
(autoload 'hl-spotlight-mode "hl-spotlight" "hl-spotlight autoload" t)

(dolist (face-name '("blue"
                     "cyan"
                     "yellow"
                     "gold"
                     "brown"
                     "orange"
                     "red"
                     "pink"
                     "magenta"
                     "violet"
                     "purple"
                     "turquoise1"
                     "SeaGreen1"
                     "green1"
                     "yellow1"
                     "chocolate4"
                     "firebrick3"
                     "grey"
                     "DarkSlateBlue"
                     ))
  (custom-declare-face (intern (concat "hi-" face-name))
                       (list (list t (list ':weight 'bold :foreground face-name)))
                       "Face for hi-lock mode."
                       :group 'hi-lock-faces)
  (custom-declare-face (intern (concat "hi-" face-name "-h"))
                       (list (list t (list :background face-name)))
                       "Face for hi-lock mode."
                       :group 'hi-lock-faces))


(defun isearch-highlight-regexp ()
  (interactive)
  (let ((regexp (if isearch-regexp
                    isearch-string
                  (regexp-quote isearch-string)))
        (face-name (hi-lock-read-face-name)))
  (highlight-regexp regexp face-name)))

(defun isearch-highlight-regexp-line ()
  (interactive)
  (let ((regexp (if isearch-regexp
                    isearch-string
                  (regexp-quote isearch-string)))
        (face-name (hi-lock-read-face-name)))
  (highlight-lines-matching-regexp regexp face-name)))

(define-key isearch-mode-map (kbd "M-h") 'isearch-highlight-regexp)
(define-key isearch-mode-map (kbd "M-l") 'isearch-highlight-regexp-line)

(autoload 'highlight-symbol-at-point2 "highlight-symbol" nil t)
(autoload 'highlight-symbol-remove-all "highlight-symbol" nil t)


(global-set-key [f7] 'highlight-symbol-at-point2)
(global-set-key [(control f7)] 'highlight-symbol-remove-all)

(autoload 'rainbow-mode "rainbow-mode" nil t)

;; allow persistent highlighting in every mode
(defadvice hi-lock-set-pattern (around hi-lock-add-keyword-advice (regexp face))
  (let ((font-lock-fontified t))
    ad-do-it))
(ad-activate 'hi-lock-set-pattern)


(defun logs-error ()
  (interactive)
  (highlight-lines-matching-regexp "[Ww][Aa][Rr][Nn]" 'hi-orange)
  (highlight-lines-matching-regexp "ERR" 'hi-red)
  (highlight-lines-matching-regexp "[Ee][Rr][Rr][Oo][Rr]" 'hi-red))
