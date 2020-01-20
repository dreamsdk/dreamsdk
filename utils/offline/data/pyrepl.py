# http://code.activestate.com/recipes/277753-find-and-replace-string-in-all-files-in-a-director/

import fileinput, glob, string, sys, os
from os.path import join

if len(sys.argv) < 3:
    print "usage: %s <search_text> <replace_text> <directory>" % os.path.basename(sys.argv[0])
    sys.exit(0)

stext = sys.argv[1]
rtext = sys.argv[2]
path = sys.argv[3]

#print "finding: \"" + stext + "\", replacing with \"" + rtext + "\", in: \"" + path + "\""

for dname, dirs, files in os.walk(path):
    for fname in files:
        fpath = os.path.join(dname, fname)
#        print fpath
        with open(fpath) as f:
            s = f.read()
        s = s.replace(stext, rtext)
        with open(fpath, "w") as f:
            f.write(s)
