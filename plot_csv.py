#!/usr/bin/env python

import datetime
import sys
import matplotlib.pyplot as plt
from matplotlib.backends.backend_pdf import PdfPages
import matplotlib 
import re
import os
import datetime
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

style = 'seaborn-darkgrid'
try:
    plt.style.use( style )
except:
    _logger.warn("Style '%s' not found. Using default" % style)

class Args: pass 
args = Args()

def getHeader(filename):
    skip_header = False
    d = args.delimiter
    hline = args.header
    if hline:
        header = hline.split( d )
        return header
    with open(filename, "r") as f:
        header = f.read().split("\n")[0]
    if '#' == header[0]:
        header = header[1:]
    header = [x.strip() for x in header.split( d )]
    _logger.debug("INFO Found header %s" % header)
    return header

def get_ycols(colexpr, header=None):
    ranges = colexpr.split(',')
    cols = []
    for r in ranges:
        if ":" in r:
            ranges = filter(None, r.strip().split(':') )
            ranges.append( len(header) - 1)
            low, high = ranges[0], ranges[1]
            cols += range(int(low), int(high))
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
    return cols

def plot_on_axes( ax, xvec, yvec, **kwargs):
    # Plot yvec, xvec on given axes.
    _logger.debug("Plotting %s" %  yvec )
    if kwargs.get('sorted', False):
        # See, http://stackoverflow.com/questions/2828059/sorting-arrays-in-numpy-by-column
        _logger.info('Sorting (xvec, yvec) so lines do not break')
        data = np.vstack(( xvec, yvec )).T
        data = data[data[:,0].argsort()]
        xvec, yvec = data[:,0], data[:,1]
    global args
    if args.plot_type == 'linear':
        ax.plot(xvec, yvec, args.marker, label = kwargs.get('label', ''))
    elif args.plot_type == 'semilogx':
        ax.semilogx(xvec, yvec, args.marker, label = kwargs.get('label', ''))
    elif args.plot_type == 'semilogy':
        ax.semilogy(xvec, yvec, args.marker, label = kwargs.get('label', ''))
    elif args.plot_type == 'loglog':
        ax.loglog(xvec, yvec, args.marker, label = kwargs.get('label', ''))
    elif args.plot_type == 'bar':
        ax.bar(xvec, yvec, label = kwargs.get('label', ''), alpha = 0.3)
    else:
        _logger.warn( "Plot type %s is not supported. " % args.plot_type )
        _logger.info( "|- Avilable: linear, semilogx, semilogy, loglog" )
        _logger.warn( " Using default" )
        ax.plot(xvec, yvec, args.marker, label = kwargs.get('label', ''))

    # ax.set_ylim( [ yvec.min() - 0.1 * (abs( yvec.min() ))
        # , yvec.max() + 0.1 * (abs( yvec.max() )) ]
        # )

    if kwargs.get('label', None):
        plt.legend(loc='best', framealpha=0.4)
    return ax

def modify_convas(header, nplots, args):
    if not args.subplot:
        return
    matplotlib.rcParams['figure.figsize'] = 10, 1.5*nplots
    matplotlib.rcParams['lines.linewidth'] = 1
    matplotlib.rcParams['font.size'] = 10
    # matplotlib.rcParams['savefig.frameon'] = False

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
    _logger.debug("Clusters (index of usecols): %s" % cluster)
    
    # Return the results clustered according to indices.
    result = []
    for k in cluster:
        c = []
        for v in cluster[k]:
            c.append(ranges.index(v))
        result.append(c)
    _logger.info("Clusters:(index of usecols) %s" % result)
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

def save_figure( outfile ):
    plt.tight_layout( )
    ext = outfile.split( '.' )[-1]
    if ext.lower() == 'pdf':
        with PdfPages( outfile ) as pdf:
            if args.pdf_note:
                with open( args.pdf_note, 'r') as note:
                    pdf.attach_note( note.read( ) )
            pdf.savefig( )
            d = pdf.infodict( )
            d['Author'] = 'Dilawar Singh'
            d['CreationDate'] = datetime.datetime.today( )
            d['ModDate'] = datetime.datetime.today( )
    else:
        plt.savefig( outfile )

def main(args):
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

    if skipheader:
        skipRows = 1
    data = None
    try:
        data = np.loadtxt(args.input_file
                , skiprows = skipRows
                , delimiter = args.delimiter
                )
    except ValueError as e:
        data = np.genfromtxt( args.input_file 
                , skip_header = skipheader
                , delimiter = args.delimiter
                )
    _logger.debug( 'Got data %s' % data )
    data = np.transpose(data)
    xvec = data[ args.xcolumn ]
    if len(usecols) > 5:
        modify_convas(header, len(usecols[1:]), args)

    if args.auto:
        ## Partition colums to reduces the numbers of subplots
        _logger.info("Clustering plots to save space --auto/-a was given")
        clusters = partition_plots(data[usecols[1:]])
        for j, subs in enumerate(clusters):
            ax = plt.subplot(len(clusters), 1, j+1)
            for i in subs:
                _logger.debug( 'Plotting %s' % i )
                # print i, usecols[i+1]
                plot_on_axes( ax, xvec, data[usecols[i+1]], label = args.header[usecols[i+1]]
                        , sorted = args.sortx
                        )
    else:
        for j, i in enumerate(usecols[1:]):
            try:
                _logger.info("Plotting %s" % args.header[i])
            except Exception as e:
                _logger.warn('Could not find anything at index %d. ingoring' % i)
                continue
            if args.subplot:
                _logger.info("plotting in subplot %s" % (j+1))
                ax = plt.subplot(len(usecols[1:]), 1, j+1, frameon=True)
            else:
                ax = plt.gca()
            plot_on_axes( ax, xvec, data[i], label = args.header[i] 
                    , sorted = args.sortx
                    )

    stamp = datetime.datetime.now().isoformat()
    if args.subplot:
        plt.suptitle( args.title or stamp + ' ' + str(args.input_file), fontsize = 8)
    else:
        plt.title(args.title or stamp + ' ' + str( args.input_file), fontsize = 8 )

    if args.header:
        plt.xlabel("%s" % args.header[args.xcolumn])

    if args.ylabel:
        plt.ylabel( args.ylabel )

    plt.tight_layout( )
    if not args.outfile:
        plt.show()
    else:
        _logger.info("Saving figure to %s" % args.outfile)
        save_figure( args.outfile )

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''A csv file plotter'''
    parser = argparse.ArgumentParser(description=description)

    parser.add_argument('--input-file', '-i', metavar='input csv file'
            , required = True
            , help = 'File to plot'
            )
    parser.add_argument('--delimiter', '-d'
            , default = ' '
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

    parser.add_argument('--plot-type', '-pt'
        , required = False
        , type = str
        , default = 'linear'
        # , action = 'store_true'
        , help = 'semilogx, semilogy, loglog, linear. Deafult: linear'
        )

    parser.add_argument('--ylabel', '-yl'
        , required = False
        , default = '' , type = str
        , help = 'Label for y-axis.'
        )

    parser.add_argument( '--pdf-note', '-pn'
            , default = None
            , help = "Txt file to attach to pdf document. Only works when "
                    " figure is saved to PDF format."
            ) 

    parser.add_argument('--sortx', '-sx'
        , action = 'store_true'
        , help = 'Sort x column before plotting. All y-cols will be sorted '
                 'accordingly'
        )
    parser.parse_args(namespace=args)
    main(args)

