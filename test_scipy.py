#!/usr/bin/env python                                                           
import timeit

setup = "import numpy;\
        import scipy.linalg as linalg;\
        x = numpy.random.random((1000,1000));\
        z = numpy.dot(x, x.T)"
count = 5

t = timeit.Timer("linalg.cholesky(z, lower=True)", setup=setup)
print( "cholesky:", t.timeit(count)/count, "sec" )

t = timeit.Timer("linalg.svd(z)", setup=setup)
print( "svd:", t.timeit(count)/count, "sec" )
