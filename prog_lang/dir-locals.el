(
 (asm-mode . ((comment-start . "/")
              ))

 ;; common c code
 (c-mode . ((tab-width . 4)
            (c-basic-offset . 4)
            (indent-tabs-mode . nil)
            ))

 ;; kernel code
 (c-mode . ((tab-width . 8)
            (c-basic-offset . 8)
            (indent-tabs-mode . t)
            (eval . (c-set-style "linux-tabs-only"))
            ))

 ;; c++
 (c++-mode . ((tab-width . 4)
              (c-basic-offset . 4)
              (indent-tabs-mode . nil)
              (eval . (c-set-offset 'innamespace 0))
              ))

 (python-mode . ((python-indent-offset . 4)
                 ))

 (js-mode . ((js-indent-level . 4)
             ))

 (web-mode . ((web-mode-markup-indent-offset . 4)
              (web-mode-code-indent-offset . 4)
              (web-mode-code-indent-offset . 4)
              ))

 (nil . ((tab-width . 4)
         (tab-stop-list . (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))
         ;;(create-lockfiles . nil)
         ))
 )
