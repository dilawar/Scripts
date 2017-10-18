#!/usr/bin/env python

"""entropy.py: 

"""

from __future__ import print_function, division
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import math

def main( ):
    args = [ eval(x.strip()) for x in sys.argv[1:] ]
    if sum( args ) <= 1.0:
        probs = args
    else:
        probs = [ x/ sum(args ) for x in args]

    assert abs(sum( probs ) - 1.0) < 1e-6, "Must add up to 1: %s" %  probs
    ent = sum( [ -x * math.log(x, 2.0) for x in probs ] )
    print( 'H=%f bits' % ent )
    return ent


if __name__ == '__main__':
    main()
