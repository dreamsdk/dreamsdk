#!/usr/bin/env python

import argparse, os

def explore_dir(dir):
    a = []
    l = os.listdir(dir)
    for d in l:
        if os.path.isdir(os.path.join(dir, d)):
            a = a + explore_dir(os.path.join(dir, d))
        else:
            a.append(os.path.join(dir, d))
    return a

# Parser
parser = argparse.ArgumentParser(description='generate a sortfile.str for mkisofs directly from a directory')
parser.add_argument('source_path', help='the target directory')
#parser.add_argument('root_path', help='root directory in sortfile.str')
args = parser.parse_args()

files = explore_dir(args.source_path)
files.sort(key=lambda f: os.path.splitext(f)[1])

for index, file in enumerate(files):
    line = file.replace("\\", "/") + " {i:d}"
    print(line.format(i = index))
