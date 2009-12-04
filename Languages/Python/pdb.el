
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
(autoload 'py-shell "python-mode" "py-shell autoload" t)
(global-set-key [(control P)] 'py-shell)
