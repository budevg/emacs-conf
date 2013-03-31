(defun replace-html-chars-region (start end)
  "Replace  “&lt;” to “<” and other chars in HTML.
This works on the current region."
  (interactive "r")
  (save-restriction 
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (search-forward "&amp;" nil t) (replace-match "&" nil t))
    (goto-char (point-min))
    (while (search-forward "&lt;" nil t) (replace-match "<" nil t))
    (goto-char (point-min))
    (while (search-forward "&gt;" nil t) (replace-match ">" nil t))
    ))
