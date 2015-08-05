#!/usr/bin/env python
"""extract_column_from_csv.py: 

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


def main(args):
    print args

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''Select columns from csv file.'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--input', '-in'
        , required = True
        , help = 'Input csv file'
        )
    parser.add_argument('--col', '-c'
        , nargs = '+'
        , help = 'Column to select. All columns matching this pattern will be selected'
        )

    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main(args)
