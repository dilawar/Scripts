#!/usr/bin/env python

"""xplot.py: 

    This program uses matplotlib to plot xplot like data.

Last modified: Fri May 23, 2014  01:40AM

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

data = {}

def buildData( file ):
    global data 
    with open(file, "r") as f:
        xvec = []
        yvec = []
        for line in f:
            if line[0] == ';' or line[0] == '#':
                continue
            line = line.strip()
            if "," in line:
                line = line.split(",")
            else:
                line = line.split()
            try:
                xvec.append(float(line[0]))
                yvec.append(line[1:])
            except:
                pass
        assert len(xvec) == len(yvec)
        data[file] = (xvec, yvec)

def zipIt(ys):
    """ Zip an n-dims vector.
    There are as many sublists as there are elements in each element of list.
    """
    result = [[ ] for x in ys[0] ]
    for y in ys:
        for i, e in enumerate(y):
            result[i].append(e)
    return result

def plotData( outFile = None ):
    global data 
    for file in data:
        xvec, yx = data[file]
        try:
            yvecs = zipIt(yx)
        except Exception as e:
            print("[FATAL] Failed to zip the given elements")
            sys.exit(0)
        for yvec in yvecs:
            pylab.plot(xvec, yvec, label='%s' % file)
    pylab.legend(loc='best', framealpha=0.4)
    if not outFile:
        pylab.show()
    else:
        print("[INFO] Saving plots to: {}".format( outFile ))
        pylab.savefig(outFile)

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("-i", "--file"
            , nargs = "+"
            , help = "xplot file to plot using matplotlib"
            )
    parser.add_argument("-o", "--output"
            , default = None
            , help = "Output file to store plot"
            )
    args = parser.parse_args()
    [ buildData(file) for file in args.file ]
    plotData( outFile = args.output )
