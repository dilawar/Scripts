#!/usr/bin/env python
"""swc_to_tikz.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import re
import networkx as nx
args_ = None

def _get_node_type( t ):
    types = { 0: 'undefined', 1 : 'soma', 2 : 'axon', 3 : 'dendrite'
            , 4 : 'apical dendrite', 5 : 'fork point', 6 : 'end point'
            , 7 : 'custom' }
    return types[int(t)]

def _parse_line( line ):
    n, T, x, y, z, R, P = line.split()
    x, y, z, R = [ float(a) for a in (x,y,z,R) ]
    n, P = int(n), int(P)
    nodeType = _get_node_type(T)
    return n, nodeType, x, y, z, R, P

def _add_coordinates( G, n, pos ):
    G.node[n]['coordinate'] = pos
    G.node[n]['coordinate2D'] = to2d(pos)
    G.node[n]['pos'] = '%f,%f!' % tuple(pos[:2])

def _print_stats( morph ):
    assert nx.is_connected( morph ), "Broken morphology"
    print( ' Number of nodes: %d' % morph.number_of_nodes() )
    print( ' Number of edges: %d' % morph.number_of_edges() )

def to2d( point ):
    if len(point) == 2:
        return point
    else:
        return tuple(point[:2])

def do_save_png( G, outfile ):
    global args_
    import matplotlib.pyplot as plt
    from mpl_toolkits.mplot3d import Axes3D

    fig = plt.figure()
    #  ax = fig.add_subplot(111, projection='3d')
    pos = nx.get_node_attributes(G, 'coordinate2D' )
    nx.draw(G, pos=pos, node_size=1)
    plt.axis('off')
    plt.title( args_.input )
    plt.savefig( outfile )

def do_action( morph, outfile ):
    global args_
    action = outfile.split('.')[-1]
    if action == 'png':
        do_save_png( morph, outfile)
        return True
    elif action == 'dot':
        nx.nx_agraph.write_dot( morph, outfile )
        return True

    raise UserWarning( 'Unknown action %s. Will do nothing.' % action )

def _sanitize_morphology(morph):
    # Housekeeping.
    # Make sure each node has 'pos' attribute
    for n in morph.nodes():
        if 'coordinate' not in morph.node[n]:
            _add_coordinates(morph, n, (0,0,0) )

def main( args ):
    global args_
    args_ = args
    morph = nx.Graph()
    for line in args_.input:
        line = line.strip()
        if not line or line[0] == '#':
            continue
        n, T, x, y, z, R, P = _parse_line(line)
        morph.add_node(n, type=T, shape='point', radius=R )
        _add_coordinates(morph, n, (x,y,z))
        if P < 0:
            continue
        morph.add_edge(P, n)
    _sanitize_morphology(morph)
    print( "[INFO ] Constructed morphology" )
    _print_stats( morph )
    outfile = args_.output or '%s.%s' % (self.input.name, self.fmt)
    do_action( morph, outfile )

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''SWC to various other formats.'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--input', '-i'
        , required = True
        , type = argparse.FileType('r')
        , help = 'Input file (swc)'
        )
    parser.add_argument('--fmt', '-f'
        , required = False, default = 'png'
        , type = str
        , help = 'Output format dot | tikz | png '
        )
    parser.add_argument('--output', '-o'
        , required = False, default = ''
        , help = 'Write result to this format.'
        )
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main( args )
