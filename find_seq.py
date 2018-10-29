#!/usr/bin/env pypy
"""find_seq.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import difflib

def main(args):
    a = args.seq
    l = len(a)
    with open( args.input, 'r') as f:
        for i, line in enumerate(f):
            for ii in len(line)-l:
                b = line[ii:ii+l]
                s = difflib.SequenceMatcher(line, a, b )
                if s.ratio >= float(args.match):
                    print("%d %s" % (i, b))

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''Find sequence using difflib (line by line)'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--input', '-i'
        , required = True
        , help = 'Input file'
        )
    parser.add_argument('--seq', '-s'
        , required = True,
        , help = 'Sequence to search'
        )
    parser.add_argument( '--similarity', '-s'
        , required = False, default = 0.9
        , type = float
        , help = 'Similarity index (1.0 means exact match)'
        )
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main( args )
