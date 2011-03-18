(autoload 'el-get-install "el-get" nil t)
(autoload 'el-get-remove "el-get" nil t)

(setq el-get-sources
      '((:name rainbow-mode
               :type git
               :url "git://git.naquadah.org/rainbow.git"
               :build ("cp rainbow-mode.el ~/.emacs.d/VisualLook")
               :after (lambda () (el-get-remove "rainbow-mode")))
        (:name psvn
               :type http
               :url "http://www.xsteve.at/prg/emacs/psvn.el"
               :build ("cp psvn.el ~/.emacs.d/SourceControl")
               :after (lambda () (el-get-remove "psvn")))
        (:name magit
               :type git
               :url "http://github.com/philjackson/magit.git"
               :build ("cp *.el ~/.emacs.d/SourceControl/magit")
               :after (lambda () (el-get-remove "magit")))
        (:name el-get
               :type git
               :url "git://github.com/dimitri/el-get.git"
               :build ("cp el-get.el ~/.emacs.d/Packages")
               :after (lambda () (el-get-remove "el-get")))
        (:name cssh
               :type git
               :url "git://github.com/dimitri/cssh.git"
               :build ("cp cssh.el ~/.emacs.d/Shell")
               :after (lambda () (el-get-remove "cssh")))
        (:name auto-complete
               :type git
               :url "git://github.com/m2ym/auto-complete.git"
               :build ("cp *.el ~/.emacs.d/Languages/SmartCode/auto-complete"
                       "cp -r dict ~/.emacs.d/Languages/SmartCode/auto-complete")
               :after (lambda () (el-get-remove "auto-complete")))
	))
