#!/usr/bin/env python
# https://stackoverflow.com/a/4944929/3726096

import argparse, mmap, os, os.path
from pathlib import Path

parser = argparse.ArgumentParser(description='determine if a Free Pascal (FPC) binary was compiled in Debug or Release mode')
parser.add_argument('binary', help='target binary to check')
args = parser.parse_args()

if os.path.isfile(args.binary) and os.access(args.binary, os.R_OK):
    dbg_filename = Path(args.binary).with_suffix('.dbg')
    with open(args.binary, 'rb', 0) as file, \
        mmap.mmap(file.fileno(), 0, access=mmap.ACCESS_READ) as s:
        if (s.find(bytes(dbg_filename)) != -1) or (s.find(b"debug_info") != -1):
            print('DEBUG')
        else:
            print('RELEASE')
else:
    raise SystemExit('error: file not found: {fname}'.format(fname = args.binary))
