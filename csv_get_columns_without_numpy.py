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

import sys

def process( data_file, columns ):
    with open( data_file, "r") as f:
        lines = f.read().split('\n')
    header, data = lines[0].split(','), lines[1:]
    colIds = []
    for column in columns:
        if column not in header:
            column = int(column)
            colIds.append( column )
        else:
            column = header.index( column )
            colIds.append( column )
    print("Fetching columns: %s" % colIds)
    for l in data:
        if not l:
            continue
        data = l.split(',')
        ys = []
        for c in colIds:
            ys.append( data[c] )
        print("\t".join(ys))

def main():
    if len(sys.argv) < 3:
        print("USAGE: %s csv_file column_name|col_id " % sys.argv[0])
        quit()
    infile = sys.argv[1]
    columns = sys.argv[2:]
    process( infile, columns)

if __name__ == '__main__':
    main()

