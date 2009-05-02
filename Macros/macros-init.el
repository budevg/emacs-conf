
(autoload 'apply-macro-to-region-lines "macros+" "macros+ autoload" t)

(defun kbd-macro-recording-action ()
  (interactive)
  (if (not (boundp 'kbd-macro-recorded))
      (setq kbd-macro-recorded nil))
  (if kbd-macro-recorded
      (progn
        (setq kbd-macro-recorded nil)
        (end-kbd-macro))
    (progn (setq kbd-macro-recorded t)
           (start-kbd-macro nil))))

(global-set-key [(f10)] 'kbd-macro-recording-action)

(global-set-key [(f11)] 'name-last-kbd-macro)
(global-set-key [(control f11)] 'edit-named-kbd-macro)

(global-set-key [(f12)] 'call-last-kbd-macro)
(global-set-key [(meta f12)] 'apply-macro-to-region-lines)
(global-set-key [(control f12)] 'edit-last-kbd-macro)

(autoload 'macro-math-eval-and-round-region "macro-math" "macros+ autoload" t)
(autoload 'macro-math-eval-region "macro-math" "macros+ autoload" t)

(global-set-key "\C-x~" 'macro-math-eval-and-round-region)
(global-set-key "\C-x=" 'macro-math-eval-region)
