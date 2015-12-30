
"""bed_parser.py: 

    This script reads the bamToBed file and outputs read counts alinged to
    a region based on the input annotation file 

"""
    
__author__           = "Anushree Narjala"
__copyright__        = "Copyright 2015, Anushree Narjala and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Anushree Narjala"
__email__            = "anushreen@ncbs.res.in"
__status__           = "Development"

import sys
import os

def file_to_list( filename ):
    text = []

    with open (filename, "r") as f:
        lines = f.read().split("\n")
    for l in lines:
        l = ','.join(l.split())
        if l: text.append(l)

    return text

def bed_parser(bedfile,annfile):
    print "Parsing %s" % bedfile
    bed = file_to_list( bedfile )
    ann = file_to_list( annfile )
    print ann


def main():
    bedfile = sys.argv[1]
    annfile = sys.argv[2]
    outfile = None
    if len(sys.argv)>3:
        outfile = sys.argv[3]
    bed_parser(bedfile,annfile)


if __name__ == '__main__':
    main()
