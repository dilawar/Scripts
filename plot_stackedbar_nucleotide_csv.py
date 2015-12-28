#!/usr/bin/env python2.6
    
__author__           = "Anushree Narjala"
__copyright__        = "Copyright 2015, Anushree Narjala and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Anushree Narjala"
__email__            = "anushreen@ncbs.res.in"
__status__           = "Development"

import numpy as np
import sys
import os
import matplotlib.pyplot as plt

def barplot_gc(csvfile):

    data = np.genfromtxt(csvfile, delimiter=',', skiprows=1)
    title = csvfile.split('/')[-1] 
    size = data.shape[0]

    plt.figure( )
    ind = np.arange( size )

    colors = [ 'tomato', 'firebrick', 'lightgreen', 'forestgreen' ]
    legends = [ 'G', 'C', 'A', 'U' ]
    width = 0.4
    bottom = np.zeros( size )
    nNuc = data[0].sum()
    plots = []
    for i, k in enumerate(data.T):
        k = 100 * k / nNuc 
        p = plt.bar(ind, k, width, bottom=bottom, color = colors[i])
        bottom += k
        plots.append( p[0] )

    plt.legend( plots, legends , bbox_to_anchor = (0, 0.87, 1, 0.11)
            , ncol = 4, mode = 'expand'
            , borderaxespad = 0.
            , fancybox = False
            )

    plt.ylim( [0, 110 ] )
    plt.xlim( [0 - width , size ] )
    plt.title("Position Specific Nucleotide Preference (%s seqs)", fontsize=15)
    plt.xlabel("Position", fontsize=13)
    plt.ylabel("Percentage", fontsize=13)
    plt.xticks( ind + width/2, [ str(x) for x in range(1, size+1) ] )

    outfile = '%s_GC.png' % csvfile
    print("Saving figures to %s" % outfile)
    plt.savefig(outfile) #, transparent=True)
    
def main( csvfile ):
    print('[INFO] Plotting %s' % csvfile)
    barplot_gc( csvfile )


if __name__ == '__main__':
    csvfile = sys.argv[1]
    main( csvfile )
