#!/usr/bin/env python

"""columns_from_csv.py: 

    Extract a given column from a csv file.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2015, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import numpy as np
import sys

def process( data_file, column ):
    with open( data_file, "r") as f:
        lines = f.read().split('\n')
    header, data = lines[0], lines[1:]
    if column not in header:
        column = int(column)
    else:
        column = header.index( column )
    print("Fetching column: %s" % column)
    for l in data:
        print(l[column])

def main():
    infile = sys.argv[1]
    column = sys.argv[2]
    process( infile, column)

if __name__ == '__main__':
    main()

