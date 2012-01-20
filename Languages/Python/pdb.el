
(defun py-pdb-pm ()
  (interactive)
  (end-of-buffer)
  (insert-string "import pdb")
  (comint-send-input)
  (insert-string "pdb.pm()")
  (comint-send-input)
  (insert-string "bt")
  (comint-send-input))

(global-set-key [(control meta p)] 'py-pdb-pm)
(autoload 'ipython "python-mode" "ipython autoload" t)
(global-set-key [(control P)] 'ipython)
