#!/usr/bin/env python3
"""
Convert a pandoc file to HTML.

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
import subprocess 
import pandoc_preprocess_doc_html

srcFile_ = None

pandoc_ = [ 'pandoc'
        , '--mathml'
        , '-F', 'pandoc-crossref'
        , '-F', 'pandoc-imagine'
        , '-F', os.path.join( os.getenv( 'HOME' ), "Scripts/pandoc/dilawar.py" )
        , '-F', 'pandoc-citeproc', '--standalone', '--self-contained'
        ]

# imgPat_ = re.compile( r'\!\[.*?\]\(?P<filename>.?\)' )
imgPat_ = re.compile( r'\!\[.*?\]\((?P<figure>.+?)\)', re.DOTALL )

def convertToPNG( img, text ):
    global srcFile_
    srcDir = os.path.dirname( srcFile_ )
    imgPath = os.path.join( srcDir, img )
    imgNameWe = '.'.join( img.split( '.' )[:-1] )
    pngPath = imgNameWe + '.png'
    if os.path.isfile( pngPath ):
        # File exits, if modification of png is later than the img then ignore.
        if os.path.getctime( pngPath ) > os.path.getctime( img ):
            text = text.replace( img, pngPath )
            return text

    print( 'Converting %s to %s' % (imgPath, pngPath) )
    cmd = [ 'convert', '-density', '300', '-quality', '90', imgPath, pngPath ]
    subprocess.check_output(cmd, shell = False )
    if not os.path.exists( pngPath ):
        print( '[WARN] to create PNG file' )
        print( '\t CMD: %s' % ' '.join( cmd ) )
    else:
        text = text.replace( img, pngPath )
    return text

def toHtml( text ):
    global srcFile_
    global pandoc_
    htmlFileNameWe = '.'.join( srcFile_.split( '.' )[:-1] )
    htmlFile = htmlFileNameWe + '.html'
    pFile = '%s_html.md' % srcFile_ 
    with open( pFile, 'wb' ) as f:
        f.write( text.encode('utf-8') )

    cmd = pandoc_ + [ '-o', htmlFile, pFile ]
    subprocess.call( cmd, shell = False )
    print( '[INFO] Wrote HTML to %s' % htmlFile )


def process( text ):
    imgs = imgPat_.findall( text )
    for img in imgs:
        ext = img.split( '.' )[-1]
        if ext.lower( ) not in [ 'png', 'jpg', 'jpeg' ]:
            text = convertToPNG( img, text )

    with open( '/tmp/%s_html' % os.path.basename( srcFile_ ), 'wb' ) as f:
        f.write( text.encode( 'utf-8' ) )
    toHtml( text )

def main( ):
    global srcFile_
    srcFile_ = sys.argv[1]
    text = pandoc_preprocess_doc_html.replace( srcFile_ )
    process( text )

if __name__ == '__main__':
    main()
