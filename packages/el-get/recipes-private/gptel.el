(:name gptel
       :description "A simple ChatGPT client for Emacs"
       :type github
       :pkgname "karthink/gptel"
       :build `(,(concat "cp *.el "
                         (in-emacs-d "applications/gpt")))
       )
