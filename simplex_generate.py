#!/usr/bin/env python

"""simplex_generate.py: 
Generate a k-simplex.

    x1 + x2 + ... + xk = 1.

USE with pypy for better performance.
"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import random 
import math
import itertools

def gen_vector_uniform( k ):
    """k-dim simplex 
    We use the simple method described here 
    https://cs.stackexchange.com/a/3229/5041
    """
    vec = [0] + sorted([random.random( ) for i in range(k)]) + [ 1.0 ]
    for i, v in enumerate( vec[:-1] ):
        vec[i] = vec[i+1] - vec[i]
    return vec[:-1]

def main( args ):
    k = args.dimension
    for i in itertools.count( ):
        v = gen_vector_uniform( k )
        print( ','.join( [ '%.5f' % x for x in v ] ) )
        if i == args.number:
            return 0
    return 0

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''Generate x1,x2,...,xk such that sum(x1,x2,...,xk)=1'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--dimension', '-d'
        , required = False, type = int, default = 3
        , help = 'Dimention of vector (default 3)'
        )
    parser.add_argument( '--number', '-N'
        , default = -1, type = int
        , help = 'Total number of vectors to generate (-1 for infinity).'
        )
    parser.add_argument('--output', '-o'
        , required = False, default = 'sys.stdout'
        , help = 'Output file'
        )
    parser.add_argument( '--method', '-m'
        , required = False, default = 'uniform'
        , help = 'Method (uniform sampling|TODO)'
        )
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main( args )
