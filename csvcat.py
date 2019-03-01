#!/usr/bin/env python3

"""
extract_column_from_csv.py: 
given a csv file and names of columns, write a new csv file.
"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2015, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import pandas as pd
import sys

def main(args):
    df = pd.read_csv(args['input'])
    df = df.filter(regex=args['column_regex'])
    if args['every'] > 1:
        df=df[::args['every']]
    outfile = args['output']
    df.to_csv(outfile, sep=args['output_delimiter']
            , float_format=args['float_format']
            , index=False)

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''Select columns from csv file.'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--input', '-i'
        , required=True, help='Input csv file'
        )
    parser.add_argument('--column-regex', '-c'
            , default = '.*'
            , help = 'Columns to select (csv list). Can contain regex.'
            )
    parser.add_argument('--output', '-out'
        , required = False
        , default = sys.stdout
        , type = argparse.FileType('w')
        , help = 'Write to this file.'
        )
    parser.add_argument('--output-delimiter', '-od'
        , required = False, default = ','
        , help = 'Output delimiter.'
        )
    parser.add_argument('--float-format', '-ff'
        , required = False, default = '%g'
        , help = 'Format of numbers.'
        )
    parser.add_argument('--every', '-e'
        , required = False, default = 1 , type=int
        , help = 'Print every nth line.'
        )
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main(vars(args))
