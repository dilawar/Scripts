#!/usr/bin/env python

from __future__ import print_function

"""bc.py: 

bc implementation in python

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
from math import *

def main( args ):
    res = eval( args.file.read( ) )
    print( res, file=sys.stdout )

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''bc: A caclulator which understands scientific notation.'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument( 'file', nargs='?', type=argparse.FileType('r'), default = sys.stdin)
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main( args )
