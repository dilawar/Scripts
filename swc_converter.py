#!/usr/bin/env python
# -*- coding: utf-8 -*-

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
import codecs
import re
import networkx as nx
import matplotlib.pyplot as plt
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
    print( ' Number of nodes: %d' % morph.number_of_nodes() )
    print( ' Number of edges: %d' % morph.number_of_edges() )

def to2d( point ):
    if len(point) == 2:
        return point
    else:
        return tuple(point[:2])

def _nx_to_paths( G ):
    # Create list of paths.
    g = G.copy()
    root = 1
    paths = []
    sinks = [ n for n, outd in list(G.out_degree()) if outd == 0 ]
    for s in sinks:
        # There is only one path from each sink to source.
        path = [s]
        edges = []
        while g.in_degree(path[-1]) > 0:
            # select a incoming vertices
            v = path[-1]
            f = list(g.predecessors(v))[0]
            edges.append( (f,v) )
            path.append(f)

        paths.append([G.node[x]['coordinate'] for x in path])
        # Remove these edges
        g.remove_edges_from(edges)
    return paths

def do_save_png( G, outfile, engine='matplotlib' ):
    fig = plt.figure()
    if engine == 'networkx':
        do_save_png_using_nx(G, outfile)
    elif engine == 'matplotlib':
        do_save_png_using_mpl3d(G, outfile)
    else:
        raise UserWarning( "Unknown engine %s" % engine )
    plt.savefig( outfile )

def _distance( p1, p2):
    d = 0
    for a, b in zip(p1,p2):
        d += (a-b)**2
    return d**0.5

def _smooth_path( path ):
    import scipy.interpolate as sci
    import numpy as np
    path = np.array(path)
    if len(path) > 3:
        X, Y, Z = path[:,0], path[:,1], path[:,2]
        tck, u = sci.splprep( [X,Y,Z])
        newX = np.linspace(0, 1, len(X))
        res = sci.splev(newX, tck)
        path = zip(*res)
    return path

def do_save_png_using_mpl3d( G, outfile ):
    global args_
    from mpl_toolkits.mplot3d import Axes3D
    ax = plt.subplot( 111, projection = '3d' )
    #  ax.view_init(90, 0)
    paths = _nx_to_paths(G)
    paths = [ _smooth_path(p) for p in paths ]
    for p in paths:
        X, Y, Z = zip(*p)
        ax.plot(X, Y, Z, color='b')
    # plot soma.
    x, y, z = G.node[1]['coordinate']
    ax.scatter( [x], [y], [z], c='red', s=G.node[1]['radius']*10)
    ax.grid( False )
    #  plt.axis('off')

def do_save_png_using_nx(G, outfile):
    global args_
    import matplotlib.pyplot as plt
    pos = nx.get_node_attributes(G, 'coordinate2D' )
    nsize = [x/1.0 for x in nx.get_node_attributes(G, 'radius' ).values()]
    nx.draw(G, pos=pos, node_size=nsize)
    plt.title( os.path.basename(args_.input.name) )
    plt.axis('off')

def write_to_csv( G, outfile ):
    import itertools
    paths = _nx_to_paths(G)
    txt = None
    for line in itertools.zip_longest(*paths, fillvalue=('','','')):
        if txt is None:
            txt = ','.join(['x{0},y{0},z{0}'.format(i) for i in range(len(line))])
        line = ','.join([ ','.join(map(str,x)) for x in line ])
        txt += '\n%s' % line
    with open(outfile, 'w' ) as f:
        f.write( txt )

def do_action( morph, outfile ):
    global args_
    action = outfile.split('.')[-1]
    if action == 'png':
        do_save_png( morph, outfile)
    elif action == 'dot':
        nx.nx_agraph.write_dot( morph, outfile )
    elif action == 'csv':
        # write all paths to csv file.
        write_to_csv( morph, outfile )
    else:
        raise UserWarning( 'Unknown action %s. Will do nothing.' % action )
        return False

    if os.path.isfile( outfile ):
        print( "[INFO ] Successfully wrote %s" % outfile )
        return True
    else:
        raise UserWarning( "Failed to write %s" % outfile )
        

def _sanitize_morphology(G):
    # Housekeeping.
    # Make sure each node has 'pos' attribute
    remove = [ n for n in G.nodes() if 'coordinate' not in G.node[n]]
    G.remove_nodes_from(remove)

def main( args ):
    global args_
    args_ = args
    morph = nx.DiGraph()
    with codecs.open( args_.input, 'r', encoding='utf-8', errors='ignore') as f:
        for line in f:
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
    _print_stats( morph )
    outfile = args_.output or '%s.%s' % (self.input.name, self.fmt)
    do_action( morph, outfile )

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''SWC to various other formats.'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--input', '-i'
        , required = True, type = str
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
