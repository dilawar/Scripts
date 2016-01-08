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
    if '#' == header[0]:
        header = header[1:]
    header = [x.strip() for x in header.split(",")]
    args.header = header
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

def plot_on_axes( ax, xvec, yvec, **kwargs):
    # Plot yvec, xvec on given axes.
    global args
    ax.plot(xvec, yvec, args.marker, label = kwargs.get('label', ''))
    ax.set_ylim( [ yvec.min() - 0.1 * (abs( yvec.min() ))
        , yvec.max() + 0.1 * (abs( yvec.max() )) ]
        )
    if kwargs.get('label', None):
        plt.legend(loc='best', framealpha=0.4)
    return ax

def modify_convas(header, nplots, args):
    if not args.subplot:
        return
    matplotlib.rcParams['figure.figsize'] = 10, 1.3*nplots
    matplotlib.rcParams['lines.linewidth'] = 1
    matplotlib.rcParams['font.size'] = 8
    #matplotlib.rcParams['savefig.frameon'] = False

def partition_plots(mat):
    """Partition plots according to min and max of columns """
    mins, maxs, avgs = [], [], []
    for i, col in enumerate(mat):
        mins.append(col.min())
        maxs.append(col.max())
        avgs.append(col.mean())

    ranges = zip(mins, maxs, avgs)
    cluster = { 0 : [ranges[0]] }
    cluster = do_clustering(ranges[1:], cluster)
    _logger.debug("Clusters: %s" % cluster)
    
    # Return the results clustered according to indices.
    result = []
    for k in cluster:
        c = []
        for v in cluster[k]:
            c.append(ranges.index(v))
        result.append(c)
    _logger.info("Clustes: %s" % result)
    return result

def mergable(x, rngs):
    import math
    merge = False
    assert type(rngs) == list
    for r in rngs:
        lx = x[1] - x[0]
        xbar = x[2]
        lr = r[1] - r[0]
        lbar = r[2]
        ratio = float(lr)/lx
        if ratio <= 0:
            return merge
        if abs(math.log(ratio, 2)) < 1.3:
            if max(x+r) - min(x+r) < abs(lr) + abs(lbar):
                merge = True
    return merge
        

def do_clustering(ranges, cluster):
    #print cluster
    if len(ranges) == 0:
        return cluster

    newranges = []
    for r in ranges:
        merged = False
        for k in cluster:
            if mergable(r, cluster[k]):
                cluster[k].append(r)
                merged = True
        if not merged:
            newranges.append(r)

    if not newranges:
        return cluster
    cluster[len(cluster)] = [newranges[0]]
    return do_clustering(newranges[1:], cluster)


def main(args):
    header = getHeader(args.input_file)
    if not header:
        skiprows = 0
    else:
        skiprows = 1

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

    try:
        data = np.loadtxt(args.input_file
                , skiprows = skiprows
                , delimiter = args.delimiter
                # , usecols = usecols
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
    _logger.debug(xvec)

    if args.auto:
        ## Partition colums to reduces the numbers of subplots
        _logger.info("Clustering plots to save space --auto/-a was given")
        clusters = partition_plots(data[usecols[1:]])
        for j, subs in enumerate(clusters):
            ax = plt.subplot(len(clusters), 1, j+1)
            for i in subs:
                yvec = data[i+1]
                plot_on_axes( ax, xvec, yvec, label = args.header[i+1])
    else:
        for j, i in enumerate(usecols[1:]):
            d = data[i]
            _logger.info("Plotting %s" % args.header[i])
            if args.subplot:
                _logger.info("plotting in subplot %s" % j)
                ax = plt.subplot(len(usecols[1:]), 1, j, frameon=True)
            else:
                ax = plt.gca()
            plot_on_axes( ax, xvec, d, label = args.header[i] )

    if args.title:
        plt.title(args.title)

    if args.header:
        plt.xlabel("%s" % args.header[0])

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
            , default = None
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

    parser.parse_args(namespace=args)
    main(args)

