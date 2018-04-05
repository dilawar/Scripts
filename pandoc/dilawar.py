#!/usr/bin/env python3

"""
Pandoc filter to process raw latex tikz environments into images.
Assumes that pdflatex is in the path, and that the standalone
package is available.  Also assumes that ImageMagick's convert
is in the path. Images are put in the tikz-images directory.

CREDIT: Did I write this? Or downloaded it from somewhere? Oh the mystery.
"""

import os
import re
import shutil
import sys
import functools
import subprocess
import hashlib
import requests
import mimetypes
import glob

from pandocfilters import toJSONFilters, Para, Image, get_filename4code, get_extension
from tempfile import mkdtemp

script_dir = os.path.dirname( __file__ )
sys.path.append( script_dir )
import theorem
import code_blocks

incomment = False

def log( *msg ):
    print( *msg, file = sys.stderr )

def get_filename( text ):
    m = hashlib.sha256( text.encode() ).hexdigest( )
    return '%s' % m


def download_image_from_url( url ):
    basename = get_filename4code( '_downloaded_from_url', url, '' )
    log( 'basename', basename )
    if os.path.isdir( basename ):
        # Return first file from this directory
        return  glob.glob( '%s/*' % basename )[0]
    try:
        r = requests.get( url, stream = True, timeout = 4)
    except Exception as e:
        return url

    ext = mimetypes.guess_extension( r.headers['content-type'] ) or url.split('.')[-1]
    if '.jpe' in ext:
        ext = '.jpg'

    os.makedirs( basename )
    filename = os.path.join( basename, 'downloaded_img' + ext )
    if not os.path.exists( filename ):
        with open( filename, 'wb' ) as f:
            f.write( r.content )
    return filename

def tikz2image(tikz_src, filetype, outfile):
    tmpdir = mkdtemp()
    olddir = os.getcwd()
    os.chdir(tmpdir)

    # Write tikz.tex file.
    with open('tikz.tex', 'w') as f:
        f.write( '\n'.join( 
            [ "\\RequirePackage{luatex85,shellesc}"
                , "\\documentclass{standalone}"
                , "\\usepackage{tikz}"
                , "\\usepackage[sfdefault]{firasans}"
                , "\\usepackage[small,euler-digits]{eulervm}"
                , "\\usepackage{pgfplots}"
                , "\\pgfplotslibrary[]{units,groupplots}"
                , "\\begin{document}" ] 
            ))
        f.write(tikz_src)
        f.write("\n\\end{document}\n")

    subprocess.call( ["latexmk", "-pdf", "-lualatex", '--shell-escape', '-silent', 'tikz.tex']
            , stdout=sys.stderr
            )
    os.chdir(olddir)
    if filetype == 'pdf':
        shutil.copyfile(tmpdir + '/tikz.pdf', outfile + '.pdf')
    else:
        call(["convert", tmpdir + '/tikz.pdf', outfile + '.' + filetype])
    shutil.rmtree(tmpdir)

def tikz(key, value, format, _):
    if key == 'RawBlock':
        [fmt, code] = value
        if fmt == "latex" and re.match( r'\begin{tikzpicture}', code):
            outfile = get_filename4code("tikz", code)
            filetype = get_extension(format, "png", html="png", latex="pdf")
            src = outfile + '.' + filetype
            if not os.path.isfile(src):
                tikz2image(code, filetype, outfile)
                log('Created image ' + src )
            return Para([Image(['', [], []], [], [src, ""])])

def comment(k, v, fmt, meta):
    global incomment
    if k == 'RawBlock':
        fmt, s = v
        if fmt == "html":
            if re.search("<!-- BEGIN COMMENT -->", s):
                incomment = True
                return []
            elif re.search("<!-- END COMMENT -->", s):
                incomment = False
                return []
    if incomment:
        return []  # suppress anything in a comment

def image_with_url( k, v, fmt, meta ):
    if k == 'Image':
        urlOrPath = v[2][0]
        if 'http' in urlOrPath:
            url = urlOrPath
            path = download_image_from_url( url )
            log( "[INFO ] Replacing url %s with downloaded file %s" % (url, path) )
            v[2][0] = path
            return Image( *v )

if __name__ == "__main__":
    toJSONFilters( 
        [ image_with_url, comment, theorem.theorems, tikz, code_blocks.codeblocks ] 
        ) 
