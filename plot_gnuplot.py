#!/usr/bin/env python

# A quick plotting of csv files.
import sys
import re
import os
import datetime
import subprocess

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
gnuplot_ = []

def getHeader(filename):
    skip_header = False
    d = args.delimiter
    hline = args.header
    if hline:
        header = hline.split(',')
        return header
    with open(filename, "r") as f:
        header = f.readlines(1)[0]
    if '#' == header[0]:
        header = header[1:]
    header = [x.strip() for x in header.split(",")]
    _logger.debug("INFO Found header %s" % header)
    return header

def get_ycols(colexpr, header=None):
    ranges = colexpr.split(',')
    cols = []
    for r in ranges:
        if "-" in r:
            low, high = r.split('-')
            cols += range(int(low), int(high)+1)
        else:
            # If not an integer, then search in column for a match. Must match
            # with column name.
            try:
                cols.append(int(r))
            except:
                assert header
                try:
                    cols.append(header.index(r))
                except:
                    print("[WARN] Could not find %s in %s" % (r, header))
                    sys.exit(0)
    return cols

def main(args):
    global gnuplot_
    header = getHeader(args.input_file)
    if args.header:
        skipheader = False
    else:
        skipheader = True

    args.header = header
    if not args.ycolumns:
        usecols = [ args.xcolumn ]
    else:
        ycols = get_ycols(args.ycolumns, header)
        usecols = [ args.xcolumn ] + ycols
    labels = []
    try:
        labels = [ args.header[i] for i in usecols ]
    except:
        labels = args.header

    _logger.info("Using columns: %s" % usecols)
    _logger.info("lables: %s" % labels)

    gnuplot_.append('set datafile separator "%s"' % args.delimiter)

    if args.outfile:
        gnuplot_.append('set terminal %s' % (args.outfile.split('.')[-1]))
        gnuplot_.append('set output "%s"' % args.outfile)

    if args.subplot:
        gnuplot_.append('set multiplot layout %s,1 title "%s"' % (len(usecols)-1,
            args.title))
        for c in usecols[1:]:
            gnuplot_.append('set title "%s"' % args.header[c])
            gnuplot_.append('unset key')
            gnuplot_.append('plot "%s" using 1:%d with lines' % (args.input_file, c+1))
    else:
        gnuplot_.append('plot "%s" using %s with lines' % (
           args.input_file, ":".join([ str(x) for x in usecols ] ) )
           )
    script = "\n".join(gnuplot_)
    with open('/tmp/gnuplot.tmp', 'w') as f:
        f.write(script)
    plot = subprocess.Popen(['gnuplot'], stdin=subprocess.PIPE)
    plot.communicate(script)
    print("Done plotting with gnuplot")

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''A csv file plotter using gnuplot'''
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
            , default = ""
            , type = str
            , help = "CSV line as header. If not give, first line is treated "
            " as header"
            )
    parser.add_argument('--xcolumn', '-x'
            , default =  0
            , type = int
            , help = 'Which column is x-axis'
            )
    parser.add_argument('--ycolumns', '-y'
            , default = "1"
            , type = str
            , help = "Columns to plot on y-axis. Index or names of columns."
            )
    parser.add_argument('--outfile', '-o'
            , required = False
            , help = 'Save plot as this file'
            )

    parser.add_argument('--marker', '-m'
            , required = False
            , default = '-'
            , type = str
            , help  = 'Which marker to use in matplotlib. default "-"'
            )

    parser.add_argument('--title', '-t'
            , required = False
            , default = ""
            , type = str
            , help = 'Tile for plot'
            )

    parser.add_argument('--subplot', '-s'
            , action = 'store_true'
            , help = 'Plot each plot as subplot'
            )

    parser.add_argument('--auto', '-a'
            , action = 'store_true'
            , help = 'Cluster subplots together according to range'
            )

    parser.add_argument('--semilogx'
        , required = False
        , action = 'store_true'
        , help = 'Semilogx on x-axis?'
        )
    parser.parse_args(namespace=args)
    main(args)

