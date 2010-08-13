import os
import sys
        

class LispWriter(object):
    SCREEN_CONFIGURE_CODE = '''
(defun maximize-frame ()
  (set-frame-position (selected-frame) %s %s)
  (set-frame-size (selected-frame) %s %s))

(add-hook \'window-setup-hook (lambda ()
  (set-frame-font "%s")
  (maximize-frame)))
'''

    def __init__(self, path):
        self.fd = open(path, "w")

    def close(self):
        self.fd.close()

    def set_root(self, path):
        self._setq("ROOT-PATH", path)

    def load_file(self, path):
        self.fd.write("(load-file \"%s\")\n" % path)

    def add_search_path(self, path):
        self.fd.write("(setq load-path (append (list \"%s\") load-path))\n" % path)

    def _setq(self, name, value):
        self.fd.write('(setq \"%s\")\n' % (name, value))
        
    def set_screen_configuration(self, x, y, width, height, font):
        data = self.SCREEN_CONFIGURE_CODE % (x, y, width, height, font)
        self.fd.write(data+"\n")

    def call(self, func, *args):
        self.fd.write("(%s %s)\n" % (func, " ".join(args)))
        
    def lisp(self, s):
        self.fd.write("%s\n" % s)

                      
class PackageManager(object):
    def __init__(self, packages, screen):
        self.packages = packages
        self.screen = screen
        
    def write_init_file(self, verbose = False, profiling = False):
        lisp_writer = LispWriter("init.el")
        lisp_writer.load_file(os.path.join(self.root_directory(),
                                           "package-manager.el"))
        for directory in self.directories():
            lisp_writer.add_search_path(directory)

        lisp_writer.set_screen_configuration(self.screen.x,
                                             self.screen.y,
                                             self.screen.width,
                                             self.screen.height,
                                             self.screen.font)
        if profiling:
            lisp_writer.lisp("(setq results-buffer (create-file-buffer \"results.txt\"))")
        for lisp_file in self.lisp_files():
            if profiling:
                lisp_writer.lisp("(package-manager-timer t)")
            lisp_writer.load_file(lisp_file)
            if profiling:
                lisp_writer.lisp("(princ (format \"%f %s\\n\" (/ (package-manager-timer nil) (float 1000000)) \"" + \
                                 lisp_file + "\" ) results-buffer)")
        if profiling:
            lisp_writer.lisp("(princ (format \"%s\\n\" (package-manager-get-load-time)) results-buffer)")
            lisp_writer.lisp("(switch-to-buffer results-buffer)")
        lisp_writer.call("package-manager-show-load-time")
        lisp_writer.close()

    def root_directory(self):
        return os.path.realpath(".")
    def directories(self):
        directories = []
        for package in self.packages:
            for element in package:
                path = os.path.join(self.root_directory(), element.path)
                if os.path.isdir(path):
                    directories.append(path)
                elif os.path.dirname(path) != self.root_directory():
                    directories.append(os.path.dirname(path))
        ret = list(set(directories))
        ret.sort()
        return ret

    def lisp_files(self):
        files = []
        for package in self.packages:
            for element in package:
                path = os.path.join(self.root_directory(), element.path)
                if path.endswith(".el"):
                    files.append(path)
        return files


                

    
        
        
COMMANDS = ["init_file",
            ]

def usage():
    print "Usage: %s <command>" % sys.argv[0]
    print "commands:"
    for command in COMMANDS:
        print "\t%s" % command
    exit(1)
    
if __name__ == "__main__":
    if len(sys.argv) < 2 or sys.argv[1] not in COMMANDS:
        usage()
        sys.exit(1)
    command = sys.argv[1]
    args = sys.argv[2:]
    if command == "init_file":
        verbose = False
        profiling = False
        if "verbose" in args:
            verbose = True
        if "profiling" in args:
            profiling = True

        from ConfigurationNG import PACKAGES, SCREEN
        pm = PackageManager(PACKAGES, SCREEN)
        pm.write_init_file(verbose=verbose, profiling = profiling)
            

    
    
