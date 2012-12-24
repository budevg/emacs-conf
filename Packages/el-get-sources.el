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
               :url "http://github.com/magit/magit.git"
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
               :url "https://github.com/auto-complete/auto-complete.git"
               :build ("git submodule update --init"
                       "cp *.el ~/.emacs.d/Languages/SmartCode/auto-complete"
                       "cp lib/fuzzy/fuzzy.el ~/.emacs.d/Languages/SmartCode/auto-complete"
                       "cp lib/popup/popup.el ~/.emacs.d/Languages/SmartCode/auto-complete"
                       "cp -r dict ~/.emacs.d/Languages/SmartCode/auto-complete")
               :after (lambda () (el-get-remove "auto-complete")))
        (:name yasnippet
               :type git
               :url "git://github.com/capitaomorte/yasnippet.git"
               :build ("cp *.el ~/.emacs.d/Languages/SmartCode/yasnippet"
                       "cp -r snippets ~/.emacs.d/Languages/SmartCode/yasnippet")
               :after (lambda () (el-get-remove "yasnippet")))
        (:name judge-indent
               :type git
               :url "http://github.com/yascentur/judge-indent-el.git"
               :build ("cp *.el ~/.emacs.d/Languages/")
               :after (lambda () (el-get-remove "judge-indent")))
        (:name eclim
               :type git
               :url "http://github.com/senny/emacs-eclim.git"
               :build ("cp `find . -name \"*.el\" | xargs` ~/.emacs.d/Languages/Java/eclim"
                       "cp -r snippets ~/.emacs.d/Languages/Java/eclim")
               :after (lambda () (el-get-remove "eclim")))
        (:name deft
               :type git
               :url "git://jblevins.org/git/deft.git"
               :build ("cp deft.el ~/.emacs.d/Publishing")
               :after (lambda () (el-get-remove "deft")))
        (:name scala-mode
               :type git
               :url "https://github.com/scala/scala-dist.git"
               :build ("cp tool-support/src/emacs/*.el ~/.emacs.d/Languages/Scala")
               :after (lambda () (el-get-remove "scala-mode")))
        (:name scala-mode2
               :type git
               :url "git://github.com/hvesalai/scala-mode2.git"
               :build ("cp *.el ~/.emacs.d/Languages/Scala")
               :after (lambda () (el-get-remove "scala-mode2")))
        (:name haskell-mode
               :type git
               :url "git://github.com/haskell/haskell-mode.git"
               :build ("cp *.el ~/.emacs.d/Languages/Haskell")
               :after (lambda () (el-get-remove "haskell-mode")))

	))
