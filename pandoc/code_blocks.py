#!/usr/bin/env python

from __future__ import print_function

"""
Pandoc filter to process code blocks with class "graphviz", "chemfig",
"standalone" into pdf images.

Dependencies:
    - pygraphviz
    - lualatex
"""

import os
import sys
import re
import subprocess

from pandocfilters import toJSONFilter, Para, Image, get_filename4code, get_caption, get_extension


def replace_ext( filename, newext = 'tex' ):
    oldExt = filename.split( '.' )[-1]
    return re.sub( r'(.+?)\.%s$' % oldExt, '\1\.%s' % newext )

def gen_standalone( code, dest ):
    tex = [ '\\RequirePackage{luatex85,shellesc}' ]
    tex += [ '\\documentclass[trim,tikz,multi=false]{standalone}' ]
    tex += [ '\\usepackage{chemfig}' ]
    tex += [ '\\usepackage{pgfplots}' ]
    tex += [ '\\begin{document}' ]
    tex += [ code ]
    tex += [ '\\end{document}']

    dirname = os.path.dirname( dest )
    basename = os.path.basename( dest )
    nameWE = '.'.join( basename.split( '.' )[:-1] )
    texFile = os.path.join( dirname, nameWE + '.tex' )

    # Write file
    with open( texFile, 'w' ) as f:
        f.write( '\n'.join( tex ) )

    res = subprocess.check_output( 
            [ 'lualatex', '--shell-escape', texFile ]
            , shell=True 
            , stderr = subprocess.STDOUT 
            , cwd = dirname
            )
    

def codeblocks(key, value, format, _):
    if key == 'CodeBlock':
        process( value, format )


def process( value, format ):
    [[ident, classes, keyvals], code] = value
    if "graphviz" in classes:
        caption, typef, keyvals = get_caption(keyvals)
        filetype = get_extension(format, "png", html="png", latex="pdf")
        dest = get_filename4code("graphviz", code, filetype)

        if not os.path.isfile(dest):
            import pygraphviz
            g = pygraphviz.AGraph(string=code)
            g.layout()
            g.draw(dest)
            sys.stderr.write('Created image ' + dest + '\n')

        return Para([Image([ident, [], keyvals], caption, [dest, typef])])

    elif "chemfig" in classes:
        caption, typef, keyvals = get_caption(keyvals)
        filetype = get_extension(format, "pdf", html="png", latex="pdf")
        dest = get_filename4code("chemfig", code, filetype)
        if not os.path.isfile(dest):
            g = gen_standalone(code, dest)
            sys.stderr.write('Created image ' + dest + '\n')

        return Para([Image([ident, [], keyvals], caption, [dest, typef])])


if __name__ == "__main__":
    toJSONFilter( codeblocks )
