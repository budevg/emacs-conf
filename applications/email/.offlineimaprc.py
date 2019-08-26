#!/usr/bin/python
import re, os
def get_password_emacs(machine, login, port):
    s = "machine %s login %s port %s password ([^ ]*)" % (machine, login, port)
    p = re.compile(s)
    authinfo = os.popen("gpg -q --no-tty -d ~/.authinfo.gpg").read()
    return p.search(authinfo).group(1).strip()
