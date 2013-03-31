(autoload 'el-get-install "el-get" nil t)
(autoload 'el-get-remove "el-get" nil t)

(setq el-get-sources
      `((:name rainbow-mode
               :type git
               :url "git://github.com/emacsmirror/rainbow-mode.git"
               :build (,(concat "cp rainbow-mode.el "
                                (in-emacs-d "look_and_feel")))
               :after (lambda () (el-get-remove "rainbow-mode")))
        (:name psvn
               :type http
               :url "http://www.xsteve.at/prg/emacs/psvn.el"
               :build (,(concat "cp psvn.el "
                                (in-emacs-d "source_control")))
               :after (lambda () (el-get-remove "psvn")))
        (:name magit
               :type git
               :url "http://github.com/magit/magit.git"
               :build (,(concat "cp *.el "
                               (in-emacs-d "source_control/magit")))
               :after (lambda () (el-get-remove "magit")))
        (:name el-get
               :type git
               :url "git://github.com/dimitri/el-get.git"
               :build (,(concat "cp el-get.el "
                                (in-emacs-d "packages")))
               :after (lambda () (el-get-remove "el-get")))
        (:name auto-complete
               :type git
               :url "https://github.com/auto-complete/auto-complete.git"
               :build ("git submodule update --init"
                       ,(concat "cp -r *.el lib/fuzzy/fuzzy.el lib/popup/popup.el dict "
                                (in-emacs-d "prog_lang/smart-code/auto-complete")))
               :after (lambda () (el-get-remove "auto-complete")))
        (:name yasnippet
               :type git
               :url "git://github.com/capitaomorte/yasnippet.git"
               :build (,(concat "cp -r *.el snippets "
                                (in-emacs-d "prog_lang/smart_code/yasnippet")))
               :after (lambda () (el-get-remove "yasnippet")))
        (:name judge-indent
               :type git
               :url "http://github.com/yascentur/judge-indent-el.git"
               :build (,(concat "cp *.el "
                                (in-emacs-d "prog_lang")))
               :after (lambda () (el-get-remove "judge-indent")))
        (:name eclim
               :type git
               :url "http://github.com/senny/emacs-eclim.git"
               :build (,(concat "cp -r snippets `find . -name \"*.el\" | xargs` "
                                (in-emacs-d "prog_lang/java/eclim")))
               :after (lambda () (el-get-remove "eclim")))
        (:name deft
               :type git
               :url "git://jblevins.org/git/deft.git"
               :build (,(concat "cp deft.el "
                                (in-emacs-d "doc_lang")))
               :after (lambda () (el-get-remove "deft")))
        (:name scala-mode
               :type git
               :url "https://github.com/scala/scala-dist.git"
               :build (,(concat "cp tool-support/src/emacs/*.el "
                                (in-emacs-d "prog_lang/scala")))
               :after (lambda () (el-get-remove "scala-mode")))
        (:name scala-mode2
               :type git
               :url "git://github.com/hvesalai/scala-mode2.git"
               :build (,(concat "cp *.el "
                                (in-emacs-d "prog_lang/scala")))
               :after (lambda () (el-get-remove "scala-mode2")))
        (:name haskell-mode
               :type git
               :url "git://github.com/haskell/haskell-mode.git"
               :build (,(concat "cp *.el "
                                (in-emacs-d "prog_lang/haskell")))
               :after (lambda () (el-get-remove "haskell-mode")))
        (:name clojure-mode
               :type git
               :url "git://github.com/technomancy/clojure-mode.git"
               :build (,(concat "cp *.el "
                                (in-emacs-d "prog_lang/clojure")))
               :after (lambda () (el-get-remove "clojure-mode")))
	))
