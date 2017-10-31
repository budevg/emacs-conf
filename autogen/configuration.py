
class Node(object):
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)

    def __call__(self):
        return self.__dict__


lisp_lib = [Node(path = "lisp_lib/init.el",
                 msg  = "Misc elisp function"),
            Node(path = "lisp_lib/idle-require.el",
                 msg  = "Load packages when emacs idle"),
            ]


look_and_feel = [Node(path = "look_and_feel/fonts.el",
                      msg  = "fonts colors"),
                 Node(path = "look_and_feel/themes",
                      msg  = "various color themes"),
                 Node(path = "look_and_feel/frame.el",
                      msg = "emacs frame confgiuration"),
                 Node(path = "look_and_feel/general-highlight.el",
                      msg = "highlight text peaces"),
                 Node(path = "look_and_feel/display-clock.el",
                      msg = "display clock"),
                 Node(path = "look_and_feel/htmlfontify-init.el",
                      msg = "create html representation of the buffer"),
                ]


navigation = [Node(path = "navigation/init.el",
                   msg = "misc initializations"),
              Node(path = "navigation/dired-init.el",
                   msg = "initialize dired related stuff"),
              Node(path = "navigation/markers.el",
                   msg = "place markers inside barkers and jump between them"),
              Node(path = "navigation/buffer-manipulate.el",
                   msg = "basic operation on buffers such as close/change positions etc ..."),
              Node(path = "navigation/source-browse.el",
                   msg = "provides source code browsing support"),
              Node(path = "navigation/hide-show.el",
                   msg = "enable show hide support"),
              Node(path = "navigation/symbols-navigate.el",
                   msg = "navigate symbols with ido mode"),
              Node(path = "navigation/expand-region",
                   msg = "expand region package"),
              ]


macros = [Node(path = "macros/init.el",
               msg = "basic macros operation record/replay etc ..."),
          ]

shell = [Node(path = "shell/shell-buffer.el",
              msg = "basic commands on shell buffer"),
         Node(path = "shell/eshell-conf.el",
              msg = "eshell configurations"),
         Node(path = "shell/man-pages.el",
              msg = "man and info pages"),
         Node(path = "shell/ssh-utils.el",
              msg = "ssh utils"),
         ]

editing = [Node(path = "editing/init.el",
                msg = "commong editing settings"),
           Node(path = "editing/dos2unix.el",
                msg = "dos to unix and vice versa converions"),
           Node(path = "editing/multiple-cursors/init.el",
                msg = "multiple cursors support"),
           ]

doc_lang = [Node(path = "doc_lang/init.el",
                 msg = "init publishing stuff"),
            Node(path = "doc_lang/org/org-init.el",
                 msg = "init org mode"),
            Node(path = "doc_lang/org/lisp",
                 msg = ""),
            Node(path = "doc_lang/org/contrib/lisp",
                 msg = ""),
            Node(path = "doc_lang/auctex/auctex-init.el",
                 msg = "init auctex mode"),
            Node(path = "doc_lang/artist-mode-init.el",
                 msg = "artist mode initialization"),
            Node(path = "doc_lang/rst-init.el",
                 msg = "configure reStructuredText-documents"),
            ]


prog_lang = [Node(path = "prog_lang/comments.el",
                  msg = "provides comments shortcuts for all programming languages modes"),
             Node(path = "prog_lang/compiler.el",
                  msg = "compiler definitions"),
             Node(path = "prog_lang/debug/init.el",
                  msg = "debugging definitions"),
             Node(path = "prog_lang/source-code.el",
                  msg = "general source code configuration"),
             Node(path = "prog_lang/scratch.el",
                  msg = "scratch buffer for each mode"),
             Node(path = "prog_lang/documentation/init.el",
                  msg = "initialize documentation modules"),
             Node(path = "prog_lang/smart_code/init.el",
                  msg = "initialize smart code modules"),
             Node(path = "prog_lang/smart_code/syntax_checking/init.el",
                  msg = "syntax checking for various languages"),
             Node(path = "prog_lang/smart_code/yasnippet/init.el",
                  msg = "initialize yasnippet"),
             #Node(path = "prog_lang/smart_code/auto-complete/init.el",
             #     msg = "initialize auto completion"),
             Node(path = "prog_lang/smart_code/company-mode/init.el",
                  msg = "initialize company-mode"),
             Node(path = "prog_lang/cc/init.el",
                  msg = "init c/c++ mode"),
             Node(path = "prog_lang/java/eclim",
                  msg = "eclim - eclipse plugin sources"),
             Node(path = "prog_lang/java/init.el",
                  msg = "init java mode"),
             Node(path = "prog_lang/javascript/init.el",
                  msg = "init java script mode"),
             Node(path = "prog_lang/lisp/slime",
                  msg = "Slime mode for lisp"),
             Node(path = "prog_lang/lisp/init.el",
                  msg = "init lisp mode"),
             Node(path = "prog_lang/perl/perl.el",
                  msg = "perl mode"),
             Node(path = "prog_lang/clojure/init.el",
                  msg = "clojure mode"),
             Node(path = "prog_lang/D/init.el",
                  msg = "D mode"),
             Node(path = "prog_lang/python/init.el",
                  msg = "init python mode"),
             Node(path = "prog_lang/ruby/init.el",
                  msg = "init ruby mode"),
             Node(path = "prog_lang/erlang/init.el",
                  msg = "init erlang mode"),
             Node(path = "prog_lang/go/init.el",
                  msg = "init go mode"),
             Node(path = "prog_lang/lua/lua-init.el",
                  msg = "init lua mode"),
             Node(path = "prog_lang/html/init.el",
                  msg = "init html mode"),
             Node(path = "prog_lang/matlab/init.el",
                  msg = "init matlab mode"),
             Node(path = "prog_lang/assembly/init.el",
                  msg = "init assembly mode"),
             Node(path = "prog_lang/scala/init.el",
                  msg = "init scala mode"),
             Node(path = "prog_lang/markdown/init.el",
                  msg = "init markdown mode"),
             Node(path = "prog_lang/haskell/init.el",
                  msg = "init haskell mode"),
             Node(path = "prog_lang/docker/init.el",
                  msg = "docker file mode"),
             Node(path = "prog_lang/groovy/init.el",
                  msg = "groovy mode"),
             Node(path = "prog_lang/rust/init.el",
                  msg = "rust mode"),
             ]

build_system = [Node(path = "build_system/init.el",
                     msg = "initialzie build system modes"),
                ]


source_control = [Node(path = "source_control/magit/init.el",
                       msg = "load magit"),
                  Node(path = "source_control/init.el",
                       msg = "load git/svn support"),
                  Node(path = "source_control/local.el",
                       msg = "local files diff"),
                  ]

applications = [Node(path = "applications/init.el",
                     msg = "misc applications"),
                Node(path = "applications/irc.el",
                     msg = "irc configration"),
                Node(path = "applications/email/init.el",
                     msg = "Email configration"),
                Node(path = "applications/prodigy-apps.el",
                     msg = "Different services to run"),
                ]

packages = [Node(path = "packages/el-get",
                 msg = ""),
            Node(path = "packages/init.el",
                 msg = "load elpa packages"),
            ]

PACKAGES = [lisp_lib,
            look_and_feel,
            editing,
            navigation,
            macros,
            shell,
            doc_lang,
            prog_lang,
            build_system,
            source_control,
            applications,
            packages,
            ]

FRAME = Node(x=0,
             y=26,
             height=51,
             width =159,
             font="-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
