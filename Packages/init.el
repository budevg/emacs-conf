(run-with-idle-timer 5 nil '(lambda () (when
                                           (load "package.el")
                                         (package-initialize))))
