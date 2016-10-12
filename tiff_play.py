#!/usr/bin/env python 

"""
play tiff file.

Convert all tiff files from a directory into a big numpy array.

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
import os
import matplotlib.pyplot as plt
import numpy as np
import logging
import cv2

args_ = None

def get_frame_data( frame ):
    try:
        img = np.array(frame)
    except Exception as e:
        img = np.array(frame.convert('L'))
    return to_grayscale(img)

def to_grayscale( img ):
    if len(img.shape) == 3:
        img = np.dot( img[...,:3], [ 0.299, 0.587, 0.114 ] )

    if img.max() >= 256.0:
        logging.debug("Converting image to grayscale")
        logging.debug("Max=%s, min=%s, std=%s"% (img.max(), img.min(),
            img.std()))
        img = 255 * ( img / float( img.max() ))
    gimg = np.array(img, dtype=np.uint8)
    return gimg

def get_bounding_box( ):
    bbox = [ int(x) for x in e.args_.box.split(',') ]
    r1, c1, h, w = bbox

    if h == -1: r2 = h
    else: r2 = r1 + h
    if w == -1: c2 = w
    else: c2 = c1 + w
    return (r1, c1, r2, c2)

def read_frames_from_avi( filename ):
    import cv2
    cap = cv2.VideoCapture( filename )
    frames = []
    while cap.isOpened():
        try:
            ret, frame = cap.read()
        except Exception as e:
            print("Failed to read frame. Error %s" % e)
            quit()
        if ret:
            gray = cv2.cvtColor( frame, cv2.COLOR_BGR2GRAY)
            frames.append( gray )

    logging.info("Total %s frames read" % len(frames))
    return frames


def read_frames_from_tiff( filename, **kwargs ):
    from PIL import Image
    tiff = Image.open( filename )
    frames = []
    try:
        i = 0
        while 1:
            i += 1
            tiff.seek( tiff.tell() + 1 )
            framedata = get_frame_data( tiff )
            # bbox = get_bounding_box( )
            # if bbox:
                # framedata = framedata[bbox[0]:bbox[2], bbox[1]:bbox[3]]
            # if kwargs.get('min2zero', False):
                # framedata = framedata - framedata.min()
            frames.append( framedata )
    except EOFError as e:
        logging.info("Total frames read from file %d" % i )
    return frames

def read_frames( videofile, **kwargs ):
    ext = videofile.split('.')[-1]
    if ext in [ 'tif', 'tiff' ]:
        return read_frames_from_tiff( videofile, **kwargs )
    elif ext in [ 'avi', 'mp4' ]:
        return read_frames_from_avi ( videofile, **kwargs )
    else:
        logging.error('Invalid format file %s is not supported yet' % ext )
        quit()

def process_file( infile, plot = True ):
    frames = read_frames( infile )
    if plot:
        plt.imshow( np.mean(frames, axis=2), interpolation = 'none', aspect = 'auto' )
        plt.colorbar( )
        outfile = '%s_avg.png' % infile
        plt.savefig( outfile )
        print( '[INFO] Wrote summay of tiff to %s' % outfile )
    return frames

def main( args ):
    global args_
    args_ = args
    print( '[DEBUG] Got args %s' % args )
    framesStack = [ process_file(x) for x in args.infiles ]
    numWindows = len( framesStack )
    for i, f in enumerate( framesStack[0] ):
        frames = []
        for j in range( numWindows ):
            frames.append( framesStack[0][i] )
        cv2.imshow( 'Frames', np.hstack(frames) )
        cv2.waitKey( int( 1000 / args.fps) )
    cv2.destroyAllWindows( )


if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''Play multiple tiff files together. On large file,
    it takes some time to load data into memory and play it. 
    '''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--infiles', '-i'
        , required = True
        , help = 'Input file(s). Multiple files must be space separated.'
        , nargs = '+'
        )
    parser.add_argument('--output', '-o'
        , required = False
        , help = 'Write summary (mean of all frames) in png.'
        )
    parser.add_argument( '--fps', '-f'
        , required = False
        , default = 15
        , type = int
        , help = 'Frame per seconds to play. Default 15 frames per second'
        )
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main( args )
