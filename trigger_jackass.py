#!/usr/bin/env python

"""test.py: 

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import numpy as np
import serial
import time
import sounddevice as sd 
import datetime
import glob
import shutil

datadir = os.path.join( os.environ['HOME'], 'Work', 'DATA', 'JACKASS' )

positiveData = os.path.join( os.environ['HOME'], 'Work', 'DATA', 'JACKASS', 'POSITIVE' )
if not os.path.exists( positiveData ):
    os.makedirs( positiveData )

def main( ):
    ser = serial.Serial( '/dev/ttyACM0', 38400, timeout = 0.5 )
    try:
        ser.write( 'P'.encode( ) )
        list_of_files = glob.glob('%s/*.png' % datadir) 
        latest_file = max(list_of_files, key=os.path.getctime)
        # More the last spectogram file to positive 
        shutil.copyfile( latest_file, positiveData )
    except KeyboardInterrupt as e:
        print( "keyboad interrupt" )
        ser.close()
    except Exception as e:
        ser.close( )
        print( e )

    try:
        ser.close()
    except Exception as e:
        pass

if __name__ == '__main__':
    main()
