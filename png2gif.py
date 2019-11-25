#!/usr/bin/env python3

import subprocess
import sys
from pathlib import Path
import argparse
# Argument parser.
description = '''Convert pngs to gif'''
parser = argparse.ArgumentParser(description=description)
parser.add_argument('input', nargs='+', help='Input files or dir')
parser.add_argument('--every',
                    '-e',
                    required=False,
                    default=1,
                    type=int,
                    help="Use every nth file")
parser.add_argument('--output',
                    '-o',
                    required=False,
                    default='animation.gif',
                    help='Output file')


class Args:
    pass


args = Args()
parser.parse_args(namespace=args)

inputs = args.input
if len(inputs) == 1:
    # it is a directory?
    inputs = [str(x) for x in Path(inputs[0]).glob('**/*.png')]
inputs = inputs[::args.every]
print(f"[INFO ] Total {len(inputs)} files found.")
pngStr = ' '.join(inputs)
cmd = f"convert {pngStr} {args.output}".split()
subprocess.run(cmd, timeout=60)
