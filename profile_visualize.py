#!/usr/bin/env python
"""profile_visualize.py: 

Visualize cProfile/profile data.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import pstats

def main():
    s = pstats.Stats( sys.argv[0] )
    s.print_stats()

if __name__ == '__main__':
    main()
