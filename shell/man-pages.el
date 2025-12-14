
(use-package man
  :bind (("<f1>" . man/with-ido)
         ("S-<f1>" . info))
  :config
  (with-ido-completion man)
  )
