#!/usr/bin/env python
# http://code.activestate.com/recipes/277753-find-and-replace-string-in-all-files-in-a-director/

import argparse, fileinput, glob, string, sys, os, mimetypes

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

parser = argparse.ArgumentParser(description='find and replace a specific string in all text files in a directory')
parser.add_argument('search_text', help='text to search in all text files')
parser.add_argument('replace_text', help='remplacement text')
parser.add_argument('directory', help='target directory')
args = parser.parse_args()

stext = args.search_text
rtext = args.replace_text
path = args.directory

if not os.path.exists(path):
    raise SystemExit('error: directory not found: {path}'.format(path = path))

for dname, dirs, files in os.walk(path):
    for fname in files:
        fpath = os.path.join(dname, fname)
        if is_text_file(fpath):
            with open(fpath) as f:
                s = f.read()
            s = s.replace(stext, rtext)
            with open(fpath, "w") as f:
                f.write(s)
