# http://code.activestate.com/recipes/277753-find-and-replace-string-in-all-files-in-a-director/

import fileinput, glob, string, sys, os, mimetypes
from os.path import join

def is_text_file(fpath):
    return mimetypes.guess_type(fpath)[0] == 'text/plain'
	   
if len(sys.argv) < 3:
    print "usage: %s <search_text> <replace_text> <directory>" % os.path.basename(sys.argv[0])
    sys.exit(0)

stext = sys.argv[1]
rtext = sys.argv[2]
path = sys.argv[3]

for dname, dirs, files in os.walk(path):
    for fname in files:
        fpath = os.path.join(dname, fname)
        if is_text_file(fpath):
            with open(fpath) as f:
                s = f.read()
            s = s.replace(stext, rtext)
            with open(fpath, "w") as f:
                f.write(s)
