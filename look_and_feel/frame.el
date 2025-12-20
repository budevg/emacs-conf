(use-package zoom-frm
  :init
  (global-unset-key (kbd "C-f"))
  :bind (("C-f z" . hydra-zoom/body)
         ("C-f w" . hydra-windows/body)
         ("C-<kp-add>"      . text-scale-increase)
         ("C-<kp-subtract>" . text-scale-decrease)
         )

  :config
  (defun rename-frame ()
    (interactive)
    (let ((new-name (read-from-minibuffer "Rename frame (to new name): ")))
      (setq frame-title-format new-name)))

  (defhydra hydra-zoom ()
    "zoom"
    ("C-<left>" text-scale-decrease "[-]" :color red)
    ("C-<right>" text-scale-increase "[+]" :color red)
    ("C-<down>" text-scale-set "[=]" :color red)
    ("<left>" zoom-frm-out "-" :color red)
    ("<right>" zoom-frm-in "+" :color red)
    ("<down>" zoom-frm-unzoom "=" :color red)
    ("q" nil "cancel" :color blue))

  (defhydra hydra-windows ()
    "windows"
    ("<up>" other-window "next window" :color red)
    ("<down>" balance-windows-area "balance windows" :color red)
    ("<left>" rename-buffer "rename buffer" :color red)
    ("<right>" rename-frame "rename frame" :color red)
    ("q" nil "cancel" :color blue))

  )
