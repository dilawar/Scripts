import simplex_generate

class args:
    def __init__( self ):
        self.dimension = 3
        self.number = 100

a = args( )
vals = simplex_generate.main( a, return_values = True )

x, y, z = zip( *vals )

import matplotlib.pyplot as plt
plt.scatter( x, y )
