"""matrix.py: 

Simple matrix library. One can change the size of matrix dynamically.

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
import numpy as np


class Matrix( np.matrix ):
    """Override numpy matrix """

    def __init__(self, default = [ ] ):
        self.mat = np.matrix( default )

    def __str__( self ):
        return self.mat.__str__( )

    def __repr__( self ):
        return self.mat.__repr__( )

    def __setitem__( self, index, v ):
        # This is the core function of this class.
        i, j = index
        imax, jmax = self.mat.shape
        if i > imax:
            mat = self.mat.copy( )
            mat.resize( (i+1, jmax) )
            self.mat = mat.copy( )
            imax, jmax = self.mat.shape
            del mat
        if j > jmax:
            mat = self.mat.copy( )
            mat.resize( (imax, j+1) )
            self.mat = mat.copy( )
            print( self.mat )
            del mat

        self.mat[ i, j ] = v


def test( ):
    a = Matrix( '1 2; 3 5' )
    print( a )
    a[ 4,5 ] = 10
    print( a )
    print( a.mean( ) )


if __name__ == '__main__':
    test()
        
