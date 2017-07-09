#!/usr/bin/env python3

"""motion_correct.py:

Correct motion.

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2016, Dilawar Singh"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import shutil
import os

data_ = '_sima.sima' 

if os.path.isdir( data_ ):
    print( '[INFO] removing dir %s' % data_ )
    shutil.rmtree( data_ )

def use_sima( tifffile ):
    import sima.motion
    outfile = '%s_corrected.tif' % tifffile
    # approach = sima.motion.PlaneTranslation2D( max_displacement=[20,20] )
    approach = sima.motion.HiddenMarkov2D( 
            granularity='row', max_displacement=[20, 30], verbose=False
            )
    seqs = [ sima.Sequence.create( 'TIFF', tifffile ) ]
    dataset = approach.correct( seqs, data_ )
    try:
        dataset.export_frames( [[[ outfile ]]], fmt = 'TIFF16' )
    except Exception as e:
        print( '[WARN] Failed to correct motion. Error was %s' % e )
        quit( )

    print( '[INFO] Wrote corrected file to %s' % outfile )

def main( datafile ):
    use_sima( datafile )

if __name__ == '__main__':
    datafile = sys.argv[1]
    main( datafile )
