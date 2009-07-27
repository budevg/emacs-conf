
(autoload 'w3m "w3m" "w3m browser" t)
(eval-after-load "w3m"
  '(progn
     (define-key w3m-mode-map [left] nil)
     (define-key w3m-mode-map [(kp-4)] 'w3m-view-previous-page)
     
     (define-key w3m-mode-map [right] nil)
     (define-key w3m-mode-map [(kp-6)] 'w3m-view-this-url)

     (define-key w3m-mode-map [up] nil)
     (define-key w3m-mode-map [(kp-8)] 'w3m-previous-anchor)

     (define-key w3m-mode-map [down] nil)
     (define-key w3m-mode-map [(kp-2)] ''w3m-next-anchor)))

     