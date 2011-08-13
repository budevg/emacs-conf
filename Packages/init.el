(autoload 'package-list-packages "package" nil t)
(autoload 'package-list-packages-no-fetch "package" nil t)
(eval-after-load "package"
  '(progn 
     (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))))
