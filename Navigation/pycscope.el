(defun pycscope-index-files (top-directory)
  (interactive "DIndex files in directory: ")
  (start-process "pycscope.py" nil "~/.emacs.d/Navigation/pycscope.py" "-R"))
