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
from pandocfilters import RawBlock, CodeBlock


def latex( x ):
    return RawBlock( 'latex',  x)

def print1( *msg ):
    print( ' | '.join(msg), file=sys.stderr )

def replace_ext( filename, newext = 'tex' ):
    oldExt = filename.split( '.' )[-1]
    return re.sub( r'(.+?)\.%s$' % oldExt, '\1\.%s' % newext )

def replace_relative_filepaths( code ):
    # Usually a datafile for gnuplot or table. It may not be surrounded by \".
    # Well sniff all paths which starts with ./ and prefix them with .. since
    # these are one level up.
    filePat = re.compile( r'\.\/((.+?)\.(csv|dat|txt))', re.I )

    # We can't replace using simple str.replace function since same filepath is
    # often repeated in code. We need to find the location.
    newText, start = [], 0
    for m in filePat.finditer( code ):
        filepath = m.group(0)
        a, b = m.span()
        replaceWith = '../%s' % filepath
        newText.append( code[start:a] )
        newText.append( replaceWith )
        start = b

    newText.append( code[start:] )
    code = ''.join( newText )
    #  code = code.replace( filepath, '../%s' % filepath )
    return code

def gen_standalone( code, dest ):
    dest = os.path.realpath( dest )
    ext = dest.split( '.' )[-1]

    code = replace_relative_filepaths( code ) 

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

    # destination may not be pdf. Use imagick to convert the pdf to other format.
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

    if not os.path.isfile( dest ):
        print1( "ERROR", "%s could not be generated." % dest )
        return False

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

    elif "include" in classes:
        caption, typef, keyvals = get_caption(keyvals)
        keyvalDict = dict(keyvals)

        listing = ''
        with open( keyvalDict['src'], 'r' ) as f:
            listing = f.read() 

        lang = keyvalDict.get( 'lang', 'python')
        code = 'Failed to find any listing.'
        if 'start' in keyvalDict:
            # Assume that both start and end are line numbers.
            start = int( keyvalDict['start'] )
            end = int( keyvalDict['end'] )
            code = '\n'.join( listing.split('\n')[start:end] )
        elif 'pat' in keyvalDict:
            pat = r'%s' % keyvalDict['pat']
            print1( pat )
            m = re.search( pat, listing, re.DOTALL )
            if m:
                code = m.group(0)
            else:
                code = "Pattern '%s' not found in '%s'" % (pat, keyvalDict['src'])
        else:
            code = 'No listing found.'
        return CodeBlock([ident, [], keyvals], code)

if __name__ == "__main__":
    toJSONFilter( codeblocks )
