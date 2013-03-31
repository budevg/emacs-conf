
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


navigation = [Node(path = "Navigation/init.el",
                   msg = "misc initializations"),
              Node(path = "Navigation/dired-init.el",
                   msg = "initialize dired related stuff"),
              Node(path = "Navigation/markers.el",
                   msg = "place markers inside barkers and jump between them"),
              Node(path = "Navigation/buffer-manipulate.el",
                   msg = "basic operation on buffers such as close/change positions etc ..."),
              Node(path = "Navigation/source-browse.el",
                   msg = "provides source code browsing support"),
              Node(path = "Navigation/hide-show.el",
                   msg = "enable show hide support"),
              Node(path = "Navigation/symbols-navigate.el",
                   msg = "navigate symbols with ido mode"),
              Node(path = "Navigation/w3m/w3m-init.el",
                   msg = "init w3m"),
              ]


macros = [Node(path = "Macros/init.el",
               msg = "basic macros operation record/replay etc ..."),
          ]

shell = [Node(path = "Shell/shell-buffer.el",
              msg = "basic commands on shell buffer"),
         Node(path = "Shell/eshell-conf.el",
              msg = "eshell configurations"),
         Node(path = "Shell/man-pages.el",
              msg = "man and info pages"),
         Node(path = "Shell/ssh-utils.el",
              msg = "ssh utils"),
         ]

editing = [Node(path = "Editing/init.el",
                msg = "commong editing settings"),
           Node(path = "Editing/dos2unix.el",
                msg = "dos to unix and vice versa converions"),
           Node(path = "Editing/multiple-cursors/init.el",
                msg = "multiple cursors support"),
           ]

publishing = [Node(path = "Publishing/init.el",
                   msg = "init publishing stuff"),
              Node(path = "Publishing/org/org-init.el",
                   msg = "init org mode"),
              Node(path = "Publishing/org/lisp",
                   msg = ""),
              Node(path = "Publishing/org/contrib/lisp",
                   msg = ""),
              Node(path = "Publishing/auctex/auctex-init.el",
                   msg = "init auctex mode"),
              Node(path = "Publishing/artist-mode-init.el",
                   msg = "artist mode initialization"),
              Node(path = "Publishing/rst-init.el",
                   msg = "configure reStructuredText-documents"),

              ]


languages = [Node(path = "Languages/comments.el",
                  msg = "provides comments shortcuts for all programming languages modes"),
             Node(path = "Languages/compiler.el",
                  msg = "compiler definitions"),
             Node(path = "Languages/Debug/init.el",
                  msg = "debugging definitions"),
             Node(path = "Languages/source-code.el",
                  msg = "general source code configuration"),
             Node(path = "Languages/scratch.el",
                  msg = "scratch buffer for each mode"),
             Node(path = "Languages/Documentation/init.el",
                  msg = "initialize documentation modules"),
             Node(path = "Languages/SmartCode/yasnippet/init.el",
                  msg = "initialize yasnippet"),
             Node(path = "Languages/SmartCode/auto-complete/init.el",
                  msg = "initialize auto completion"),
             Node(path = "Languages/CC/init.el",
                  msg = "init c/c++ mode"),
             Node(path = "Languages/Java/eclim",
                  msg = "eclim - eclipse plugin sources"),
             Node(path = "Languages/Java/init.el",
                  msg = "init java mode"),
             Node(path = "Languages/JavaScript/init.el",
                  msg = "init java script mode"),
             Node(path = "Languages/Lisp/slime",
                  msg = "Slime mode for lisp"),
             Node(path = "Languages/Lisp/init.el",
                  msg = "init lisp mode"),
             Node(path = "Languages/Perl/perl.el",
                  msg = "perl mode"),
             Node(path = "Languages/Clojure/init.el",
                  msg = "clojure mode mode"),
             Node(path = "Languages/Python/pdb.el",
                  msg = "use pdb for python debugging"),
             Node(path = "Languages/Python/init.el",
                  msg = "init python mode"),
             Node(path = "Languages/Ruby/init.el",
                  msg = "init ruby mode"),
             Node(path = "Languages/Erlang/init.el",
                  msg = "init erlang mode"),
             Node(path = "Languages/Go/init.el",
                  msg = "init go mode"),
             Node(path = "Languages/Lua/lua-init.el",
                  msg = "init lua mode"),
             Node(path = "Languages/HTML/init.el",
                  msg = "init html mode"),
             Node(path = "Languages/Matlab/init.el",
                  msg = "init matlab mode"),
             Node(path = "Languages/Assembly/init.el",
                  msg = "init assembly mode"),
             Node(path = "Languages/Scala/init.el",
                  msg = "init scala mode"),
             Node(path = "Languages/Markdown/init.el",
                  msg = "init markdown mode"),
             Node(path = "Languages/Haskell/init.el",
                  msg = "init haskell mode"),
             ]

build_system = [Node(path = "BuildSystem/init.el",
                     msg = "initialzie build system modes"),
                ]


source_control = [Node(path = "SourceControl/magit/init.el",
                       msg = "load magit"),
                  Node(path = "SourceControl/init.el",
                       msg = "load git/svn support"),
                  Node(path = "SourceControl/local.el",
                       msg = "local files diff"),
                  ]

applications = [Node(path = "Applications/init.el",
                     msg = "misc applications"),
                Node(path = "Applications/irc.el",
                     msg = "irc configration"),
                ]

packages = [Node(path = "Packages/init.el",
                 msg = "load elpa packages"),
            Node(path = "Packages/el-get-sources.el",
                 msg = "load el-get packages")
            ]

email = [Node(path = "Email/init.el",
              msg = "Email configuration"),
         ]

PACKAGES = [lisp_lib,
            look_and_feel,
            editing,
            navigation,
            macros,
            shell,
            publishing,
            languages,
            build_system,
            source_control,
            applications,
            packages,
            email,
            ]

FRAME = Node(x=0,
             y=26,
             height=51,
             width =159,
             font="-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
