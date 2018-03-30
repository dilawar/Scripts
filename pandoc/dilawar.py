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

from pandocfilters import toJSONFilters, Para, Image, get_filename4code, get_extension
from tempfile import mkdtemp

script_dir = os.path.dirname( __file__ )
sys.path.append( script_dir )
import theorem
import code_blocks

incomment = False

def tikz2image(tikz_src, filetype, outfile):
    tmpdir = mkdtemp()
    olddir = os.getcwd()
    os.chdir(tmpdir)

    # Write tikz.tex file.
    with open('tikz.tex', 'w') as f:
        f.write( '\n'.join( 
            [ "\\RequirePackage{luatex85,shellesc}"
                , "\\documentclass{standalone}", "\\usepackage{tikz}"
                , "\\usepackage{libertine,mathpazo}"
                , "\\usepackage{pgfplots}", "\\begin{document}" ] 
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
        if fmt == "latex" and re.match("\\\\begin{tikzpicture}", code):
            outfile = get_filename4code("tikz", code)
            filetype = get_extension(format, "png", html="png", latex="pdf")
            src = outfile + '.' + filetype
            if not os.path.isfile(src):
                tikz2image(code, filetype, outfile)
                sys.stderr.write('Created image ' + src + '\n')
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

if __name__ == "__main__":
    toJSONFilters( [ comment, theorem.theorems, code_blocks.codeblocks, tikz ] ) 
