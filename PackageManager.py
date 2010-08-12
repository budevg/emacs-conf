import os
import sys
from Node import Package, Module

SCREEN_CONFIGURE_CODE = '''
(defun maximize-frame ()
  (set-frame-position (selected-frame) %s %s)
  (set-frame-size (selected-frame) %s %s))

(add-hook \'window-setup-hook (lambda ()
  (set-frame-font "%s")
  (maximize-frame)))
'''
        

class PackageManager(object):
    def __init__(self, root, screen):
        self.root = root
        self.screen = screen

    def build_skeleton(self, root = None):
        self.apply_on_tree(self.root, self._create_node)
    

    def apply_on_tree(self, tree, func):
        for node in tree:
            func(node)
            self.apply_on_tree(node, func)

    def _create_node(self, node):
        if isinstance(node, Package):
            if not os.path.exists(node.get_path()):
                print "Creating directory %s" % node.get_path()
                os.mkdir(node.get_path())
        elif isinstance(node, Module):
            if not os.path.exists(node.get_path()):
                print "Creating file %s" % node.get_path()
                fd = file(node.get_path(),"w")
                fd.close()

    def _elisp_profile_load_file(self, fd, file_name):
        file_name = os.path.realpath(file_name)
        
        fd.write("(package-manager-timer t)\n")
        
        self._elisp_load_file(fd, file_name)
        
        fd.write("(display-warning :debug (format \"load %s:%f\" \"" + file_name + "\" (/ (package-manager-timer nil) (float 1000000))))\n")
        
    def _elisp_load_file(self, fd, file_name):
        file_name = os.path.realpath(file_name)
        fd.write("(load-file \"%s\")\n" % file_name)

    def _elisp_prepend_path(self, fd, path):
        path = os.path.realpath(path)
        fd.write("(setq load-path (append (list \"%s\") load-path))\n" % path)
    def _elisp_show_load_time(self, fd):
        fd.write("(package-manager-show-load-time)\n")
        

    def create_init_file(self, verbose):
        init_fd = file(os.path.join(self.root.get_path(),"init.el"),"w")
        self._elisp_load_file(init_fd, "./package-manager.el")
        print >> init_fd, "(setq ROOT-PATH \"%s\")" % self.root.get_path()
        print >> init_fd, SCREEN_CONFIGURE_CODE % (self.screen.x, self.screen.y,
                                                   self.screen.width, self.screen.height,
                                                   self.screen.font)
                          
        def f(node):
            if isinstance(node, Package):
                self._elisp_prepend_path(init_fd, node.get_path())
            elif isinstance(node, Module):
                if verbose:
                    self._elisp_profile_load_file(init_fd, node.get_path())
                else:
                    self._elisp_load_file(init_fd, node.get_path())

        self.apply_on_tree(self.root, f)
        self._elisp_show_load_time(init_fd)
        init_fd.close()

    def init(self, verbose = "false"):
        if verbose == "true":
            verbose = True
        else:
            verbose = False
        self.build_skeleton()
        self.create_init_file(verbose)

    
        
        
COMMANDS = {"build_skeleton":None,
            "create_init_file":None,
            "init":None
            }

def usage():
    print "Usage: %s <command>" % sys.argv[0]
    print "commands:"
    for command in COMMANDS:
        print "\t%s" % command
    exit(1)
    
if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] not in COMMANDS:
        usage()
    from Configuration import PACKAGES, SCREEN
    pm = PackageManager(PACKAGES, SCREEN)
    getattr(pm, sys.argv[1])(*sys.argv[2:])
            

    
    
