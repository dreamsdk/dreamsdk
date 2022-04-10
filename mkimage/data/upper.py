#!/usr/bin/env python

import argparse, os

parser = argparse.ArgumentParser(description='convert all filenames into uppercase on a directory')
parser.add_argument('path', help='the target directory path')
args = parser.parse_args()

for file in os.listdir(args.path):
    os.rename(os.path.join(args.path, file), os.path.join(args.path, file.upper()))
