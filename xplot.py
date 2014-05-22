#!/usr/bin/env python

"""xplot.py: 

    This program uses matplotlib to plot xplot like data.

Last modified: Sat Jan 18, 2014  05:01PM

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
            line = line.strip()
            line = line.split()
            xvec.append(float(line[0]))
            yvec.append(line[1:])
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

def plotData( ):
    global data 
    for file in data:
        xvec, yx = data[file]
        try:
            yvecs = zipIt(yx)
        except Exception as e:
            print("[FATAL] Failed to zip the given elements")
            sys.exit(0)
        for yvec in yvecs:
            pylab.plot(xvec, yvec)
    pylab.show()

if __name__ == "__main__":
    if len(sys.argv[1:]) < 1:
        print("[FATAL] I need at least one file to plot. Quitting...")
        sys.exit(0)
    else:
        pass
        [ buildData(file) for file in sys.argv[1:] ]
    plotData( )
