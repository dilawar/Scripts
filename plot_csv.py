#!/usr/bin/env python
# A quick plotting of csv files.
import sys
import pylab
import re
import os
import numpy as np

import logging
logging.basicConfig(level=logging.DEBUG,
    format='%(asctime)s %(name)-12s %(levelname)-8s %(message)s',
    datefmt='%m-%d %H:%M',
    filename='plot_csv.log',
    filemode='w')
console = logging.StreamHandler()
console.setLevel(logging.INFO)
formatter = logging.Formatter('%(name)-12s: %(levelname)-8s %(message)s')
console.setFormatter(formatter)
_logger = logging.getLogger('')
_logger.addHandler(console)


class Args: pass 
args = Args()

def getHeader(filename):
    d = args.delimiter
    hline = args.header
    if not hline:
        return None
    with open(filename, "r") as f:
        header = f.read().split("\n")[0]
    header = header.split(",")
    args.header = header
    _logger.debug("INFO Found header %s" % header)
    return header

def main(args):
    header = getHeader(args.input_file)
    if not header:
        skiprows = 0
    else:
        skiprows = 1

    if not args.ycolumns:
        usecols = [ args.xcolumn ]
    else:
        for y in args.ycolumns:
            usecols = [ args.xcolumn ] + args.ycolumns 
    labels = [ args.header[i] for i in usecols ]
    _logger.info("[INFO] Using columns: %s" % usecols)
    _logger.debug("[INFO] lables: %s" % labels)
    data = np.loadtxt(args.input_file
            , skiprows = skiprows
            , delimiter = args.delimiter
            , usecols = usecols
            )
    data = np.transpose(data)
    xvec = data[0]
    for i, d in enumerate(data[1:]):
        _logger.info("Plotting %s" % i)
        pylab.plot(xvec, d, '.', label = labels[i+1])
        pylab.legend(loc='best', framealpha=0.4)
    if args.header:
        pylab.xlabel("%s" % labels[0])

    if not args.outfile:
        pylab.show()
    else:
        _logger.info("Saving figure to %s" % args.outfile)
        pylab.savefig(args.outfile)

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''A csv file plotter'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--input_file', '-in', metavar='input csv file'
        , required = True
        , help = 'File to plot'
        )
    parser.add_argument('--delimiter', '-d'
        , default = ','
        , help = 'Delimiter'
        )
    parser.add_argument('--header', '-t'
            , default = True
            , help = "Is first line header?"
            )
    parser.add_argument('--xcolumn', '-x'
        , default =  0
        , type = int
        , help = 'Which column is x-axis'
        )
    parser.add_argument('--ycolumns', '-y'
        , nargs = '+'
        , default = [1]
        , type = int
        , help = "Columns to plot as y-axis"
        )
    parser.add_argument('--outfile', '-o'
        , required = False
        , help = 'Save plot as this file'
        )
    parser.parse_args(namespace=args)
    main(args)
