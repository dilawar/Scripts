#!/usr/bin/env python

"""xplot.py: 

    This program uses matplotlib to plot xplot like data.

Last modified: Wed Jun 24, 2015  05:31PM

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2013, NCBS Bangalore"
__credits__          = ["NCBS Bangalore", "Bhalla Lab"]
__license__          = "GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@iitb.ac.in"
__status__           = "Development"

import sys
import pylab

plots_ = {}
args_ = None

def buildData( file ):
    global plots_ 
    with open(file, "r") as f:
        filetext = f.read()
    plots = filter(None, filetext.split("/newplot"))

    for p in plots:
        xvec, yvec = [], []
        if not p:
            continue
        p = filter(None, p.split('\n'))
        plotname, data = p[0].replace('/plotname', '').strip(), p[1:]
        plotname = plotname.split('/')[-1]
        for d in data:
            d = d.strip()
            if d:
                x, y = d.split()
                xvec.append(float(x)); yvec.append(float(y))
        plots_[plotname] = (xvec, yvec)
    return plots_

def plotData( outFile = None ):
    global plots_ 
    if args_.overlap > 0:
        pylab.figure( figsize = (10, 2*len(plots_)))
    for i, plotname in enumerate(plots_):
        if args_.overlap > 0:
            pylab.subplot(len(plots_), 1, i)
        xvec, yvec = plots_[plotname]
        pylab.plot(xvec, yvec, label=plotname)
        pylab.legend(loc='best', framealpha=0.4)

    if args_.overlap > 0:
        try:
            pylab.tight_layout()
        except: pass

    if not outFile:
        pylab.show()
    else:
        print("[INFO] Saving plots to: {}".format( outFile ))
        pylab.savefig(outFile, transparent=True)

if __name__ == "__main__":
    import argparse
    args_ = None
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--file"
            , nargs = "+"
            , help = "xplot file to plot using matplotlib"
            )

    parser.add_argument("-o", "--output"
            , default = None
            , help = "Output file to store plot"
            )

    parser.add_argument('--overlap'
            , default = 1
            , type = int
            , help = "Overlap plots "
            )

    args_ = parser.parse_args()
    [ buildData(file) for file in args_.file ]
    plotData( outFile = args_.output )
