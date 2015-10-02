


#!/usr/bin/env python
# A quick plotting of csv files.
import sys
import matplotlib.pyplot as plt
import matplotlib 
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

try:
    plt.style.use('ggplot')
except:
    _logger.warn("Style 'ggplot' not found. Using default")

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

def get_ycols(colexpr):
    ranges = colexpr.split(',')
    cols = []
    for r in ranges:
        if "-" in r:
            low, high = r.split('-')
            cols += range(int(low), int(high)+1)
        else:
            cols.append(int(r))
    return cols

def modify_convas(header, nplots, args):
    if not args.subplot:
        return
    matplotlib.rcParams['figure.figsize'] = 10, 1.3*nplots
    matplotlib.rcParams['lines.linewidth'] = 1
    matplotlib.rcParams['font.size'] = 8
    #matplotlib.rcParams['savefig.frameon'] = False

def main(args):
    header = getHeader(args.input_file)
    if not header:
        skiprows = 0
    else:
        skiprows = 1

    if not args.ycolumns:
        usecols = [ args.xcolumn ]
    else:
        ycols = get_ycols(args.ycolumns)
        usecols = [ args.xcolumn ] + ycols
    labels = []
    try:
        labels = [ args.header[i] for i in usecols ]
    except:
        labels = args.header

    _logger.info("[INFO] Using columns: %s" % usecols)
    _logger.debug("[INFO] lables: %s" % labels)

    try:
        data = np.loadtxt(args.input_file
                , skiprows = skiprows
                , delimiter = args.delimiter
                , usecols = usecols
                )
    except:
        print("[WARN] Can get given ranges. Getting default.")
        data = np.loadtxt(args.input_file
                , skiprows = skiprows
                , delimiter = args.delimiter
                )
    data = np.transpose(data)
    xvec = data[0]
    modify_convas(header, len(data), args)
    for i, d in enumerate(data[1:]):
        _logger.info("Plotting %s" % i)
        if args.subplot:
            _logger.info("plotting in subplot")
            plt.subplot(len(data[1:]), 1, i, frameon=True)
        if args.marker:
            plt.plot(xvec, d, args.marker, label = labels[i+1])
        else:
            plt.plot(xvec, d, label = labels[i+1])
        plt.legend(framealpha=0.4)

    if args.title:
        plt.title(args.title)

    if args.header:
        plt.xlabel("%s" % labels[0])

    if not args.outfile:
        plt.show()
    else:
        _logger.info("Saving figure to %s" % args.outfile)
        plt.savefig(args.outfile)

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''A csv file plotter'''
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument('--input_file', '-i', metavar='input csv file'
            , required = True
            , help = 'File to plot'
            )
    parser.add_argument('--delimiter', '-d'
            , default = ','
            , help = 'Delimiter'
            )
    parser.add_argument('--header'
            , default = True
            , help = "Is first line header?"
            )
    parser.add_argument('--xcolumn', '-xs'
            , default =  0
            , type = int
            , help = 'Which column is x-axis'
            )
    parser.add_argument('--ycolumns', '-y'
            , default = ","
            , type = str
            , help = "Columns to plot as y-axis"
            )
    parser.add_argument('--outfile', '-o'
            , required = False
            , help = 'Save plot as this file'
            )

    parser.add_argument('--marker', '-m'
            , required = False
            , default = None
            , type = str
            , help  = 'Which marker to use in matplotlib'
            )

    parser.add_argument('--title', '-t'
            , required = False
            , default = None
            , type = str
            , help = 'Tile for plot'
            )

    parser.add_argument('--subplot', '-s'
            , action = 'store_true'
            , help = 'Plot each plot as subplot'
            )

    parser.parse_args(namespace=args)
    main(args)

