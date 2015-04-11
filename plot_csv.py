#!/usr/bin/env python
# A quick plotting of csv files.

import sys
import pylab
import os
from collections import defaultdict

class Args: pass 
args = Args()

xvec = defaultdict(list)
yvecs = defaultdict(list)

def plotLines(header, lines):
    xindex = args.xcolumn
    yindices = args.ycolumns
    print yindices
    for l in lines:
        l = l.split(",")


def main(args):
    with open(args.input_file, "r") as f:
        txt = f.read()
    lines = txt.split("\n")
    lines = filter(None, [l.strip() for l in lines])
    plotLines(lines[0], lines[1:])


if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''A csv file plotter'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--input_file', '-in', metavar='input csv file'
            , required = True
            , help = 'File to plot'
            )
    parser.add_argument('--xcolumn', '-x'
        , default =  0
        , type = int
        , help = 'Which column is x-axis'
        )
    parser.add_argument('--ycolumns', '-y'
            , nargs = '+'
            , type = int
            , help = "Colums to plot as y-axis"
            )
    parser.parse_args(namespace=args)
    main(args)
