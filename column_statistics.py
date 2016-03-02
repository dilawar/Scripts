#!/usr/bin/env python

# Get a column from standard output and do some statistics.

import sys
import numpy as np

def main():
    txt = sys.stdin.read()
    data = []
    lines = txt.split('\n')
    for x in lines:
        try: 
            data.append( float(x.strip() ) )
        except:
            pass
    if len(lines) > len(data):
        print("[WARN] Could not convert %s lines to float" % (len(lines) - len(data)))
    data = np.array( data, np.float )

    print( "N   : %s" % len(data) )
    print( "Sum : %s" % data.sum() )
    print( "Mean: %s" % data.mean() )
    print( "Std : %s" % data.std( ) )

if __name__ == '__main__':
    main()
