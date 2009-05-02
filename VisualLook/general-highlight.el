
(autoload 'highlight-80+-mode
  "highlight-80+" "highlight-80+ autoload" t)

(global-set-key [(control shift b)] 'highlight-80+-mode)

(autoload 'highlight-parentheses-mode
  "highlight-parentheses" "highlight-parentheses autoload" t)

(add-hook 'emacs-lisp-mode-hook
	  (lambda ()
	    (highlight-parentheses-mode t)))
(add-hook 'html-mode-hook
	  (lambda ()
	    (highlight-parentheses-mode t)))
(add-hook 'c-mode-hook
          (lambda ()
	    (highlight-parentheses-mode t)))
(add-hook 'c++-mode-hook
          (lambda ()
	    (highlight-parentheses-mode t)))

(add-hook 'cperl-mode-hook
          (lambda ()
	    (highlight-parentheses-mode t)))

(add-hook 'java-mode-hook
          (lambda ()
	    (highlight-parentheses-mode t)))

(autoload 'highline-mode "highline" "highline autoload" t)
(autoload 'hl-spotlight-mode "hl-spotlight" "hl-spotlight autoload" t)

(defface hi-blue
  '((t (:weight bold :foreground "blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-blue-h
  '((t (:background "blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-cyan
  '((t (:weight bold :foreground "cyan")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-cyan-h
  '((t (:foreground "black" :background "cyan")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-yellow
  '((t (:weight bold :foreground "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-yellow-h
  '((t (:foreground "black" :background "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-gold
  '((t (:weight bold :foreground "gold")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-gold-h
  '((t (:foreground "black" :background "gold")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-brown
  '((t (:weight bold :foreground "brown")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-brown-h
  '((t (:background "brown")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-orange
  '((t (:weight bold :foreground "orange")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-orange-h
  '((t (:background "orange")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-red
  '((t (:weight bold :foreground "red")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-red-h
  '((t (:background "red")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-pink
  '((t (:weight bold :foreground "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-pink-h
  '((t (:foreground "black" :background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-magenta
  '((t (:weight bold :foreground "magenta")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-magenta-h
  '((t (:background "magenta")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-violet
  '((t (:weight bold :foreground "violet")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-violet-h
  '((t (:foreground "black" :background "violet")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-purple
  '((t (:weight bold :foreground "purple")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-purple-h
  '((t (:background "purple")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-turquoise1
  '((t (:weight bold :foreground "turquoise1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-turquoise1-h
  '((t (:foreground "black" :background "turquoise1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-SeaGreen1
  '((t (:weight bold :foreground "SeaGreen1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-SeaGreen1-h
  '((t (:foreground "black" :background "SeaGreen1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-green1
  '((t (:weight bold :foreground "green1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-green1-h
  '((t (:foreground "black" :background "green1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-yellow1
  '((t (:weight bold :foreground "yellow1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-yellow1-h
  '((t (:foreground "black" :background "yellow1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-chocolate4
  '((t (:weight bold :foreground "chocolate4")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-chocolate4-h
  '((t (:background "chocolate4")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-firebrick3
  '((t (:weight bold :foreground "firebrick3")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-firebrick3-h
  '((t (:background "firebrick3")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-grey
  '((t (:weight bold :foreground "grey")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-grey-h
  '((t (:foreground "black" :background "grey")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-DarkSlateBlue
  '((t (:weight bold :foreground "DarkSlateBlue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-DarkSlateBlue-h
  '((t (:background "DarkSlateBlue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
