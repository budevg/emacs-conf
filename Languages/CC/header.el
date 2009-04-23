(defun c-insert-header-protector ()
  (interactive)
  (let ((header-protector (format "__%s__"
                                  (replace-regexp-in-string
                                   "[\\.]" "_"
                                   (upcase
                                    (file-name-nondirectory buffer-file-name))))))
    (beginning-of-buffer)
    (insert (format "\n#ifndef %s\n" header-protector))
    (insert (format "#define %s\n" header-protector))
    (end-of-buffer)
    (insert (format "\n#endif //%s\n" header-protector))))
