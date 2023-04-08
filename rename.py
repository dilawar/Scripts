#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""rename.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import pathlib
import re

class Args: pass 
args_ = Args()

def split_pat(pat):
    pat = pat.split('/')
    return pat[1], pat[2]

def replace_name(s, pat):
    f, t = split_pat(pat)
    return re.sub(f, t, s)

def main():
    global args_
    pat = split_pat(args_.pattern)
    todo = {}
    for f in args_.files:
        todo[f] = replace_name(f, args_.pattern)
    for k, v in todo.items():
        if args_.dry_run:
            print( f'{k} will be renamed to {v}')
            continue
        k = pathlib.Path(k).rename(v)

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''Rename files (like PERL-rename)'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('pattern'
        , help = 'Pattern e.g. "s/pat1/replace/g" '
        )
    parser.add_argument('--dry-run', '-n'
        , required = False, action = 'store_true'
        , help = 'Dry run (show what will happen)'
        )
    parser.add_argument('files', nargs='+', help = 'FILEs')
    parser.parse_args(namespace=args_)
    main()
