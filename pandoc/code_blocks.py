#!/usr/bin/env python3

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

from pandocfilters import toJSONFilter, Para
from pandocfilters import Image, get_filename4code, get_caption, get_extension
from pandocfilters import RawBlock


def latex( x ):
    return RawBlock( 'latex',  x)

def print1( *msg ):
    print( msg, file=sys.stderr )

def replace_ext( filename, newext = 'tex' ):
    oldExt = filename.split( '.' )[-1]
    return re.sub( r'(.+?)\.%s$' % oldExt, '\1\.%s' % newext )

def gen_standalone( code, dest ):
    dest = os.path.realpath( dest )
    ext = dest.split( '.' )[-1]

    code = r'\[ %s \]' % code

    tex = [ '\\RequirePackage{luatex85,shellesc}' ]
    tex += [ '\\documentclass[preview,multi=false]{standalone}' ]
    tex += [ '\\usepackage{amsmath,amssymb}' ]
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

    res1 = subprocess.check_output( 
            [ 'lualatex', '-shell-escape', texFile ]
            , shell=False, stderr = subprocess.STDOUT 
            , cwd = dirname
            )

    if ext != 'pdf':
        pdfFile = os.path.join( dirname, nameWE + '.pdf' )
        outfile = os.path.join( dirname, nameWE + '.%s' % ext )
        print1( pdfFile, outfile )
        opts = '-density 300 -antialias -quality 100'. split( )
        res = subprocess.check_output( 
                [ 'convert', pdfFile ] + opts + [ outfile ]
                , shell=False
                , stderr = subprocess.STDOUT
                , cwd = dirname
                )

    assert os.path.isfile( dest ), "%s could not be generated." % dest
    

def codeblocks(key, value, format, _):
    if key == 'CodeBlock':
        return process( value, format )

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

    elif "standalone" in classes:
        #  print( 'Found standalone', file = sys.stderr, end = ' ' )
        if format == "latex":
            #  print( ' writer latex', file = sys.stderr )
            # if writer is latex, there is no need to generate spearate
            # standalone figure. Embed into latex itself.
            newCode = r'\[ \label{%s}' % ident if ident else ''
            newCode += '\n%s \\]' % code
            return latex( newCode )

        caption, typef, keyvals = get_caption(keyvals)
        filetype = get_extension(format, "png", html="png", latex="pdf")
        dest = get_filename4code("standalone", code, filetype)
        if not os.path.isfile(dest):
            gen_standalone(code, dest)
            sys.stderr.write('Created image ' + dest + '\n')
        else:
            sys.stderr.write('Exists  ' + dest + '\n')

        return Para([Image([ident, [], keyvals], caption, [dest, typef])])

if __name__ == "__main__":
    toJSONFilter( codeblocks )
