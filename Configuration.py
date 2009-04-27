from Node import Package,Module


faces = Package(name = "VisualLook",
                elements = [Module(name = "fonts.el",
                                   description  = "configures fonts look"),
                            Module(name = "window.el",
                                   description = "emacs window look"),
                            Module(name = "general-highlight.el",
                                   description = "highlight text peaces"),
                            Module(name = "display-clock.el",
                                   description = "display clock"),
                            ])

navigation = Package(name = "Navigation",
                     elements = [Module(name = "init.el",
                                        description = "misc initializations"),
                                 Module(name = "buffer-rotate.el",
                                        description = "provides rotation between buffers"),
                                 Module(name = "markers.el",
                                        description = "place markers inside barkers and jump between them"),
                                 Module(name = "speedbar-init.el",
                                        description = "configure emacs speedbar for quick navigation"),
                                 Module(name = "buffer-manipulate.el",
                                        description = "basic operation on buffers such as close/change positions etc ..."),
                                 Module(name = "source-browse.el",
                                        description = "provides source code browsing support"),
                                 Module(name = "hide-show.el",
                                        description = "enable show hide support"),
                                 Module(name = "symbols-navigate.el",
                                        description = "navigate symbols with ido mode"),
                                 ])

macros = Package(name = "Macros",elements =
                 [Module(name = "macros-init.el",
                         description = "basic macros operation record/replay etc ..."),
                  ])

shell = Package(name = "Shell",
                elements = [Module(name = "shell-buffer.el",
                                   description = "basic commands on shell buffer"),
                            Module(name = "eshell-conf.el",
                                   description = "eshell configurations"),
                            ])

editing = Package(name = "Editing",
                  elements = [Module(name = "common-settings.el",
                                     description = "commong editing settings"),
                              Module(name = "dos2unix.el",
                                     description = "dos to unix and vice versa converions"),
                              Module(name = "rectangle.el",
                                     description = "Edit text rectangles in emacs"),
                              ])
documentation = Package(name = "Documentation",
                        elements = [Module(name = "man-pages.el",
                                           description = "man pages shortcuts"),
                                    Package(name = "org",
                                        elements = [Module(name = "org-init.el",
                                                           description = "init org mode"),
                                                    ]),
                                    Package(name = "auctex",
                                        elements = [Module(name = "auctex-init.el",
                                                           description = "init auctex mode"),
                                                    ]),
                                    ])



languages = Package(name = "Languages",
                    elements = [Module(name = "comments.el",
                                       description = "provides comments shortcuts for all programming languages modes"),
                                Module(name = "compiler.el",
                                       description = "compiler definitions"),
                                Module(name = "debug-ng.el",
                                       description = "debugging definitions"),
                                Module(name = "source-code.el",
                                       description = "general source code configuration"),
                                Package(name = "Documentation",
                                        elements = [Module(name = "init.el",
                                                           description = "initialize documentation modules"),
                                                    ]),
                                Package(name = "Templates",
                                        elements = [Module(name = "init.el",
                                                           description = "initialize templates"),
                                                    ]),
                                Package(name = "CC",
                                        elements = [Module(name = "init.el",
                                                           description = "init c/c++ mode"),
                                                    Module(name = "header.el",
                                                           description = "c/c++ headers functions"),
                                                    ]),
                                Package(name = "Java",
                                        elements = [Module(name = "init.el",
                                                           description = "init java mode"),
                                                    ]),
                                Package(name = "Lisp",
                                        elements = [Module(name = "scratch-buffer.el",
                                                           description = "prepare the scratch buffer"),
                                                    Module(name = "init.el",
                                                           description = "init lisp mode")]),
                                Package(name = "Perl",
                                        elements = [Module(name = "perl.el",
                                                           description = "perl mode"),
                                                    ]),
                                Package(name = "Python",
                                        elements = [Module(name = "pdb.el",
                                                           description = "use pdb for python debugging"),
                                                    Module(name = "init.el",
                                                           description = "init python mode"),
                                                    Package(name = "Pymacs",
                                                            elements = [Module(name = "init.el",
                                                                               description = "init pymacs mode"),
                                                                        ]),
                                                    ]),
                                Package(name = "Ruby",
                                        elements = [Module(name = "init.el",
                                                           description = "init ruby mode"),
                                                    ]),
                                ])

source_control = Package(name = "SourceControl",
                         elements = [Module(name = "init.el",
                                            description = "load git/svn support"),
                                     Module(name = "local.el",
                                            description = "local files diff"),
                                     ])
applications = Package(name = "Applications",
                         elements = [Module(name = "irc.el",
                                            description = "irc configration"),
                                     Module(name = "twitter-init.el",
                                            description = "twitter interfaces"),
                                     ])


GLOBAL_PACKAGE = Package(name = ".",
                         elements = [faces,
                                     editing,
                                     navigation,
                                     macros,
                                     shell,
                                     documentation,
                                     languages,
                                     source_control,
                                     applications,
                                     ])

