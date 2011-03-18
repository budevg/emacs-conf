class Bunch(object):
    def __init__(self, **kwargs):
        self.__dict__.update(kwargs)



elisp = [Bunch(path = "Elisp/message-notify.el",
               msg = "Show alerts and notifications"),
         Bunch(path = "Elisp/idle-require.el",
               msg  = "Load packages when emacs idle"),
         ]


faces = [Bunch(path = "VisualLook/fonts.el",
               msg  = "fonts colors"),
         Bunch(path = "VisualLook/frame.el",
               msg = "emacs frame confgiuration"),
         Bunch(path = "VisualLook/general-highlight.el",
               msg = "highlight text peaces"),
         Bunch(path = "VisualLook/display-clock.el",
               msg = "display clock"),
         Bunch(path = "VisualLook/htmlfontify-init.el",
               msg = "create html representation of the buffer"),
         ]


navigation = [Bunch(path = "Navigation/init.el",
                    msg = "misc initializations"),
              Bunch(path = "Navigation/dired-init.el",
                    msg = "initialize dired related stuff"),
              Bunch(path = "Navigation/markers.el",
                    msg = "place markers inside barkers and jump between them"),
              Bunch(path = "Navigation/buffer-manipulate.el",
                    msg = "basic operation on buffers such as close/change positions etc ..."),
              Bunch(path = "Navigation/source-browse.el",
                    msg = "provides source code browsing support"),
              Bunch(path = "Navigation/hide-show.el",
                    msg = "enable show hide support"),
              Bunch(path = "Navigation/symbols-navigate.el",
                    msg = "navigate symbols with ido mode"),
              Bunch(path = "Navigation/w3m/w3m-init.el",
                    msg = "init w3m"),
              ]


macros = [Bunch(path = "Macros/init.el",
                msg = "basic macros operation record/replay etc ..."),
          ]

shell = [Bunch(path = "Shell/shell-buffer.el",
               msg = "basic commands on shell buffer"),
         Bunch(path = "Shell/eshell-conf.el",
               msg = "eshell configurations"),
         Bunch(path = "Shell/man-pages.el",
               msg = "man and info pages"),
         Bunch(path = "Shell/ssh-utils.el",
               msg = "ssh utils"),
         ]

editing = [Bunch(path = "Editing/init.el",
                 msg = "commong editing settings"),
           Bunch(path = "Editing/dos2unix.el",
                 msg = "dos to unix and vice versa converions"),
           ]

publishing = [Bunch(path = "Publishing/org/org-init.el",
                    msg = "init org mode"),
              Bunch(path = "Publishing/org/lisp",
                    msg = ""),
              Bunch(path = "Publishing/org/contrib/lisp",
                    msg = ""),
              Bunch(path = "Publishing/auctex/auctex-init.el",
                    msg = "init auctex mode"),
              Bunch(path = "Publishing/artist-mode-init.el",
                    msg = "artist mode initialization"),
              ]


languages = [Bunch(path = "Languages/comments.el",
                   msg = "provides comments shortcuts for all programming languages modes"),
             Bunch(path = "Languages/compiler.el",
                   msg = "compiler definitions"),
             Bunch(path = "Languages/debug-ng.el",
                   msg = "debugging definitions"),
             Bunch(path = "Languages/source-code.el",
                   msg = "general source code configuration"),
             Bunch(path = "Languages/scratch.el",
                   msg = "scratch buffer for each mode"),
             Bunch(path = "Languages/Documentation/init.el",
                   msg = "initialize documentation modules"),
             Bunch(path = "Languages/SmartCode/yasnippet/init.el",
                   msg = "initialize yasnippet"),
             Bunch(path = "Languages/SmartCode/auto-complete/init.el",
                   msg = "initialize auto completion"),
             Bunch(path = "Languages/CC/init.el",
                   msg = "init c/c++ mode"),
             Bunch(path = "Languages/Java/init.el",
                   msg = "init java mode"),
             Bunch(path = "Languages/JavaScript/init.el",
                   msg = "init java script mode"),
             Bunch(path = "Languages/Lisp/slime",
                   msg = "Slime mode for lisp"),
             Bunch(path = "Languages/Lisp/init.el",
                   msg = "init lisp mode"),
             Bunch(path = "Languages/Perl/perl.el",
                   msg = "perl mode"),
             Bunch(path = "Languages/Clojure/init.el",
                   msg = "clojure mode mode"),
             Bunch(path = "Languages/Python/pdb.el",
                   msg = "use pdb for python debugging"),
             Bunch(path = "Languages/Python/init.el",
                   msg = "init python mode"),
             Bunch(path = "Languages/Ruby/init.el",
                   msg = "init ruby mode"),
             Bunch(path = "Languages/Erlang/init.el",
                   msg = "init erlang mode"),
             Bunch(path = "Languages/Go/init.el",
                   msg = "init go mode"),
             Bunch(path = "Languages/Lua/lua-init.el",
                   msg = "init lua mode"),
             Bunch(path = "Languages/HTML/init.el",
                   msg = "init html mode"),
             Bunch(path = "Languages/Matlab/init.el",
                   msg = "init matlab mode"),
             ]

build_system = [Bunch(path = "BuildSystem/init.el",
                      msg = "initialzie build system modes"),
                ]


source_control = [Bunch(path = "SourceControl/magit/init.el",
                        msg = "load magit"),
                  Bunch(path = "SourceControl/init.el",
                        msg = "load git/svn support"),
                  Bunch(path = "SourceControl/local.el",
                        msg = "local files diff"),
                  ]

applications = [Bunch(path = "Applications/init.el",
                      msg = "misc applications"),
                Bunch(path = "Applications/irc.el",
                      msg = "irc configration"),
                ]

packages = [Bunch(path = "Packages/init.el",
                  msg = "load elpa packages"),
            Bunch(path = "Packages/el-get-sources.el",
                  msg = "load el-get packages")
            ]


PACKAGES = [elisp,
            faces,
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
            ]

SCREEN = Bunch(x=0,
               y=26,
               height=51,
               width =159,
               font="-unknown-DejaVu Sans Mono-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
