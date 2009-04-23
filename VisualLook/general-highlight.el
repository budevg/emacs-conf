
(require 'highlight-80+)

(global-set-key [(control shift b)] 'highlight-80+-mode)

(require 'highlight-parentheses)

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

(require 'highline)
(require 'hl-spotlight)

(defface hi-blue
  '((t (:background "blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-blue-b
  '((t (:weight bold :foreground "blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-cyan
  '((t (:background "cyan")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-cyan-b
  '((t (:weight bold :foreground "cyan")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-yellow
  '((t (:background "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-yellow-b
  '((t (:weight bold :foreground "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-gold
  '((t (:background "gold")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-gold-b
  '((t (:weight bold :foreground "gold")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-brown
  '((t (:background "brown")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-brown-b
  '((t (:weight bold :foreground "brown")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-orange
  '((t (:background "orange")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-orange-b
  '((t (:weight bold :foreground "orange")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-red
  '((t (:background "red")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-red-b
  '((t (:weight bold :foreground "red")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-pink
  '((t (:background "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-pink-b
  '((t (:weight bold :foreground "pink")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-magenta
  '((t (:background "magenta")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-magenta-b
  '((t (:weight bold :foreground "magenta")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-violet
  '((t (:background "violet")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-violet-b
  '((t (:weight bold :foreground "violet")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-purple
  '((t (:background "purple")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-purple-b
  '((t (:weight bold :foreground "purple")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-turquoise1
  '((t (:background "turquoise1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-turquoise1-b
  '((t (:weight bold :foreground "turquoise1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-SeaGreen1
  '((t (:background "SeaGreen1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-SeaGreen1-b
  '((t (:weight bold :foreground "SeaGreen1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-green1
  '((t (:background "green1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-green1-b
  '((t (:weight bold :foreground "green1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-yellow1
  '((t (:background "yellow1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-yellow1-b
  '((t (:weight bold :foreground "yellow1")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-chocolate4
  '((t (:background "chocolate4")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-chocolate4-b
  '((t (:weight bold :foreground "chocolate4")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-firebrick3
  '((t (:background "firebrick3")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-firebrick3-b
  '((t (:weight bold :foreground "firebrick3")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-grey
  '((t (:background "grey")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-grey-b
  '((t (:weight bold :foreground "grey")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-DarkSlateBlue
  '((t (:background "DarkSlateBlue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
(defface hi-DarkSlateBlue-b
  '((t (:weight bold :foreground "DarkSlateBlue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)
