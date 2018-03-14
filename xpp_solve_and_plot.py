#!/usr/bin/env python

import sys
import xppy    # pip3 install git+https://github.com/dilawar/xppy

import matplotlib as mpl
import matplotlib.pyplot as plt

def main( args ):
    system = args.input
    print( 'Solving system in file %s' % system )
    a = xppy.run( system, verbose = True )
    desc = a.getDesc( )
    data = a.getRawData( )

    tvec = a[:,desc['time']]
    if not args.no_subplots:
        ax = plt.subplot(111)

    i = 0
    for k in desc:
        if type(desc[k]) == int:
            if k in [ 't', 'time' ]:
                continue
            if args.no_subplots:
                ax = plt.subplot( data.shape[1] - 1, 1, i + 1 )
                i += 1
            ax.plot( tvec, data[:,desc[k]], label = k )
            ax.legend( )

    outfile = args.output or '%s.png' % args.input
    plt.savefig( outfile )
    print( 'Saved to %s' % outfile )

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''Solve and plot XPP files'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--input', '-i'
        , required = True 
        , help = 'XPP file. Must have all paramters.'
        )
    parser.add_argument('--output', '-o'
        , required = False, default = ''
        , help = 'Output PNG file. Default infile.png'
        )
    parser.add_argument('--no-subplots', '-s'
        , required = False, action = 'store_false'
        , help = 'Disable subplot(s) in final image'
        )
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main( args )
