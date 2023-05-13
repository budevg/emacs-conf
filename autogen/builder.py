import os
import re
import sys
import argparse
from contextlib import contextmanager
from configuration import Node, PACKAGES, FRAME

def out(msg):
    print msg

def unique(key):
    def wrap(f):
        def g(self, *args, **kwargs):
            dup = {}
            for elem in f(self, *args, **kwargs):
                if getattr(elem, key) not in dup:
                    dup[getattr(elem, key)] = 1
                    yield elem
        return g
    return wrap

class ConfigBuilder(object):
    def __init__(self, action, packages, frame):
        self._action = action
        self._packages = packages
        self._frame = frame
        self._init_data = []
        self._early_init_data = []
        self._data = self._init_data

    def build(self):
        if self._action == "create":
            self._create()
        if self._action == "skeleton":
            self._skeleton()

    @contextmanager
    def _early_config(self):
        self._data = self._early_init_data
        yield
        self._data = self._init_data

    def _skeleton(self):
        for pkg_dir in self._enum_dirs():
            if not os.path.exists(pkg_dir.dir):
                print pkg_dir.dir
                os.makedirs(pkg_dir.dir)
    def _newline(self):
        self._data.append("")

    def _create(self):
        with self._early_config():
            self._set_load_early_init()
            self._optimize_startup_time()
            self._native_compilation_flags()
            self._newline()
            self._gen_config_path()
            self._newline()
            self._gen_exec_path()
            self._newline()
            self._gen_tmpdir()
            self._newline()
            self._gen_env()
            self._newline()
            self._gen_frame_config()

        self._ensure_load_early_init()
        self._gen_load_packages()
        self._newline()
        self._write_init_files()

    def _native_compilation_flags(self):
        self._data.append('''
(setq comp-enable-subr-trampolines nil)''')

    def _set_load_early_init(self):
        self._data.append('''
        (setq early-init-loaded t)
        '''.lstrip())

    def _ensure_load_early_init(self):
        self._data.append('''
        (if (not (boundp 'early-init-loaded)) (load-file (concat user-emacs-directory "early-init.el")))
        '''.lstrip())

    def _optimize_startup_time(self):
        self._data.append('''
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6
      file-name-handler-alist-save file-name-handler-alist
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (with-current-buffer "*scratch*"
              (insert (format ";; Emacs loaded in %.3f sec with %d garbage collections\\n"
                              (float-time (time-subtract after-init-time before-init-time))
                              gcs-done)))
            (setq gc-cons-threshold 16777216 ; 16mb
                  gc-cons-percentage 0.1
                  file-name-handler-alist file-name-handler-alist-save)
            ))
'''.lstrip())

    def _gen_config_path(self):
        config_path = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
        self._data.append('(setq EMACS-CONFIG-PATH "%s")' % config_path)
        self._data.append('''
(defun in-emacs-d (path)
  (concat EMACS-CONFIG-PATH "/" path))
''')

    def _gen_exec_path(self):
        bashrc = os.path.expanduser("~/.bashrc")
        if not os.path.exists(bashrc):
            return

        home = os.path.expanduser("~")

        with open(bashrc) as f:
            for line in f:
                m = re.match(r"^PATH=(.*):\$PATH$", line)
                if not m:
                    continue
                path_elements = [e.replace("$HOME", home)
                                 for e in m.group(1).split(":")]
                self._data.append('(setq exec-path (append (list %s) exec-path))' %
                                  " ".join('"%s"' % e for e in path_elements))
                self._data.append('(setenv "PATH" (concat "%s:" (getenv "PATH")))' %
                                  ":".join(path_elements))

    def _gen_tmpdir(self):
        bashrc = os.path.expanduser("~/.bashrc")
        if not os.path.exists(bashrc):
            return

        home = os.path.expanduser("~")

        with open(bashrc) as f:
            for line in f:
                m = re.match(r"^export TMPDIR=(.*)$", line)
                if not m:
                    continue
                tmpdir = m.group(1).replace("$HOME", home)
                self._data.append('(setq temporary-file-directory "%s/")' % tmpdir)
                self._data.append('(setenv "TMPDIR" "%s")' % tmpdir)

    def _gen_env(self):
        for k, v in {"PAGER" : "cat"}.iteritems():
            self._data.append('(setenv "%s" "%s")' %
                              (k, v))
            self._data.append("")

    def _gen_load_packages(self):
        for pkg_dir in self._enum_dirs():
            self._data.append('''
(add-to-list \'load-path (in-emacs-d "%(dir)s"))
'''.strip() % pkg_dir())

        for d in [os.path.expanduser("~/.nix-profile/share/emacs/site-lisp")]:
            self._data.append('''
(add-to-list \'load-path "%s")
'''.strip() % d)

        for pkg_file in self._enum_files():
            self._data.append('''
(load-file (in-emacs-d "%(file)s"))
'''.strip() % pkg_file())

    @unique("file")
    def _enum_files(self):
        return self._enum_packages("file", self._packages)

    @unique("dir")
    def _enum_dirs(self):
        return self._enum_packages("dirs", self._packages)

    def _enum_packages(self, cond, packages):
        if isinstance(packages, list):
            for pkg in packages:
                for elem in self._enum_packages(cond, pkg):
                    yield elem
        else:
            pkg = packages
            if cond == "file":
                if pkg.path.endswith(".el"):
                    yield Node(file=pkg.path)
            elif cond == "dirs":
                if pkg.path.endswith(".el"):
                    yield Node(dir=os.path.dirname(pkg.path))
                else:
                    yield Node(dir=pkg.path)

    def _gen_frame_config(self):
        self._data.append('''
; (defun maximize-frame ()
;   (set-frame-position (selected-frame) %(x)d %(y)d)
;   (set-frame-size (selected-frame) %(width)d %(height)d))
''' % self._frame())
        self._data.append('''
(add-hook 'window-setup-hook (lambda ()
  (set-frame-font "%(font)s")
  (add-to-list 'default-frame-alist
               '(font . "%(font)s"))
; (maximize-frame)
  ))''' % self._frame())
        self._data.append('''
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(setq inhibit-splash-screen t
      make-pointer-invisible t)
''')

    def _write_init_files(self):
        with open("init.el", "w") as f:
            f.write("\n".join(self._init_data))
        with open("early-init.el", "w") as f:
            f.write("\n".join(self._early_init_data))



def main():
    parser = argparse.ArgumentParser(description='Autogenerate emacs configuration')
    parser.add_argument('--action',
                        dest='action',
                        action='store',
                        default='create',
                        required=True,
                        help='specify action to use during '
                             'autogeneration of emacs '
                             'configuration')

    args = parser.parse_args()
    if args.action in ['create', 'skeleton']:
        builder = ConfigBuilder(args.action, PACKAGES, FRAME)
        builder.build()
        return 0

    else:
        out('Invalid action [%s]' % args.action)
        parser.print_help()
        return 1


if __name__ == '__main__':
    status = main()
    sys.exit(status)
