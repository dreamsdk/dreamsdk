# http://code.activestate.com/recipes/277753-find-and-replace-string-in-all-files-in-a-director/

import fileinput, glob, string, sys, os, mimetypes

def is_text_file(fpath):
	base = os.path.basename(fpath)
	radical = os.path.splitext(base)[0]
	filename, file_extension = os.path.splitext(fpath)
	ftype = mimetypes.guess_type(fpath)[0]
	file_extension = file_extension.lower()
	radical = radical.lower()
	return (ftype in ['application/x-sh', 'text/plain']) or \
		(file_extension in ['.mk', '.cpp', '.s', '.awk']) or \
		(radical in ['makefile', 'doxyfile', 'readme', 'faq', 'relnotes'])
	   
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
				