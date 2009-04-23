
(if (require 'pymacs "pymacs" t)
    (progn (setq pymacs-load-path (list 
                                   (expand-file-name (concat
                                                      ROOT-PATH 
                                                      "/Languages/Python/Pymacs/modules/")))))
  (warn "package <pymacs> is missing use apt-get pymacs to install"))


