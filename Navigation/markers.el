
(require 'cl) ; common lisp extension for cadar functions

(setq place-markers '())

(defun markers-clear ()
  (interactive)
  (setq place-markers '()))



(defun markers-add-tail ()
  (interactive)
  (let ((marker (list (current-buffer) (point))))
    (if place-markers
	(nconc place-markers (list marker))
      (setq place-markers (list marker)))))


(defun markers-add-head ()
  (interactive)
  (let ((marker (list (current-buffer) (point))))
    (setq place-markers (cons marker place-markers))))


(defun markers-remove-tail ()
  (interactive)
  (if place-markers
      (setq place-markers (reverse (cdr (reverse  place-markers))))))

(defun markers-remove-head ()
  (interactive)
  (if place-markers
      (setq place-markers (cdr place-markers))))


(defun markers-switch-left ()
  (interactive)
  (if place-markers
      (let ((position (cadar place-markers))
	    (buffer (caar place-markers)))
	(markers-remove-head)
	(switch-to-buffer buffer)
	(goto-char position)
	(markers-add-tail))))

(defun markers-switch-right ()
  (interactive)
  (if place-markers
      (let ((position (cadar (reverse place-markers)))
	    (buffer (caar (reverse place-markers))))
	(markers-remove-tail)
	(switch-to-buffer buffer)
	(goto-char position)
	(markers-add-head))))



(global-set-key [(control meta down)] 'markers-add-head)
(global-set-key [(control meta up)] 'markers-remove-head)
(global-set-key [(control meta right)] 'markers-switch-right)
(global-set-key [(control meta left)] 'markers-switch-left)

(global-set-key [(control next)] 'markers-add-head)
(global-set-key [(control prior)] 'markers-remove-head)
(global-set-key [(meta prior)] 'markers-switch-right)
(global-set-key [(meta next)] 'markers-switch-left)

