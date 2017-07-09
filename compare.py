#!/usr/bin/env python
"""compare.py: 

    Compare two csv files.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2015, Dilawar Singh and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import numpy as np
import pylab

def compare(fileA, fileB):
    mooseData = np.recfromcsv(fileA, delimiter=',')
    nrnData = np.recfromcsv(fileB, delimiter=',')
    mooseData = zip(*mooseData)
    nrnData = zip(*nrnData)
    print mooseData[0]
    pylab.plot([1e3*x for x in mooseData[0]], [ 1e3*x for x in mooseData[1]]
            , label = 'moose')
    pylab.plot(nrnData[0], nrnData[1],
            label = 'neuron')
    #pylab.plot(mooseData)
    #pylab.plot(nrnData)
    pylab.show()

def main():
    mooseFile = sys.argv[1]
    nrnFile = sys.argv[2]
    compare(mooseFile, nrnFile)

if __name__ == '__main__':
    main()
