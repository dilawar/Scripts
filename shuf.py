#!/usr/bin/env python

"""shuf.py: 

Shuffle with random seed.

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

def main( ):
    seed = None
    if len( sys.argv ) > 1:
        seed = sys.argv[1]
    lines = [ ]
    for l in sys.stdin:
        lines.append( l )

    if seed is not None:
        random.seed( seed )
    random.shuffle( lines )
    print( '\n'.join( lines ) )


if __name__ == '__main__':
    main()
