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

def main( ):
    ser = serial.Serial( '/dev/ttyACM0', 38400, timeout = 0.5 )
    try:
        ser.write( 'P'.encode( ) )
    except KeyboardInterrupt as e:
        print( "keyboad interrupt" )
        ser.close()

    ser.close()

if __name__ == '__main__':
    main()
