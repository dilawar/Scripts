#!/usr/bin/env python3
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

from pandocfilters import toJSONFilter, Para, Str, Strong, RawInline
from pandocfilters import Image, get_filename4code, get_caption, get_extension
from pandocfilters import RawBlock


def latex( x ):
    return RawBlock( 'latex',  x)

def print1( *msg ):
    print( ' | '.join(msg), file=sys.stderr )

def replace_ext( filename, newext = 'tex' ):
    oldExt = filename.split( '.' )[-1]
    return re.sub( r'(.+?)\.%s$' % oldExt, '\1\.%s' % newext )

def gen_standalone( code, dest ):
    dest = os.path.realpath( dest )
    ext = dest.split( '.' )[-1]

    code = r'%s' % code

    tex = [ '\\RequirePackage{luatex85,shellesc}' ]
    tex += [ '\\documentclass[preview,multi=false]{standalone}' ]
    tex += [ '\\usepackage{amsmath,amssymb}' ]
    tex += [ '\\usepackage[sfdefault]{FiraSans}' ]
    tex += [ '\\usepackage[small,euler-digits]{eulervm}' ]
    tex += [ '\\usepackage{chemfig}' ]
    tex += [ '\\usepackage{tikz}' ]
    tex += [ '\\usetikzlibrary{calc,shapes,arrows, arrows.meta, positioning,fit}' ]
    tex += [ '\\usepackage{pgfplots}' ]
    tex += [ '\\usepgfplotslibrary{units}' ]
    if r'\begin{document}' not in code:
        tex += [ '\\begin{document}' ]
    tex += [ code ]
    if r'\end{document}' not in code:
        tex += [ '\\end{document}']

    dirname = os.path.dirname( dest )
    basename = os.path.basename( dest )
    nameWE = '.'.join( basename.split( '.' )[:-1] )
    texFile = os.path.join( dirname, nameWE + '.tex' )

    # Write file
    with open( texFile, 'w' ) as f:
        f.write( '\n'.join( tex ) )

    res1 = subprocess.run( [ 'lualatex', '-shell-escape', texFile ]
            #  , shell=False, stderr = subprocess.STDOUT
            , cwd = dirname
            , check = False
            , stdout = subprocess.PIPE, stderr = subprocess.PIPE
            )

    if res1.returncode != 0:
        print1( "WARN", "It seems previous command failed." )
        print1( '%s' % res1.stdout.decode( 'utf8' )  )
        return False

    if not os.path.isfile( dest ):
        print1( "ERROR", "%s could not be generated." % dest )
        return False

    if ext != 'pdf':
        pdfFile = os.path.join( dirname, nameWE + '.pdf' )
        outfile = os.path.join( dirname, nameWE + '.%s' % ext )
        opts = '-density 300 -antialias -quality 100'. split( )
        res = subprocess.run( 
                [ 'convert', pdfFile ] + opts + [ outfile ]
                , shell=False
                , stdout = subprocess.PIPE, stderr = subprocess.STDOUT
                , cwd = dirname
                )

    return True


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
            print1('INFO', 'Created image ' + dest + '\n')

        return Para([Image([ident, [], keyvals], caption, [dest, typef])])

    elif "standalone" in classes:
        caption, typef, keyvals = get_caption(keyvals)
        filetype = get_extension(format, "png", html="png", latex="pdf")
        dest = get_filename4code("standalone", code, filetype)
        if not os.path.isfile(dest):
            success = gen_standalone(code, dest)
            if not success:
                return Para([ Str(">>> Error: This image could not be generated.")] )
        return Para([Image([ident, [], keyvals], caption, [dest, typef])])

if __name__ == "__main__":
    toJSONFilter( codeblocks )
