#!/usr/bin/env python
from __future__ import unicode_literals

"""

"""
    
__author__           = "Me"
__copyright__        = "Copyright 2016, Me"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Me"
__email__            = ""
__status__           = "Development"

import sys
import os
import matplotlib.pyplot as plt
import numpy as np
import urllib2 
from bs4 import BeautifulSoup
import html2text
import re


def queryIntranet( query, page = 0 ):
    url =  "https://intranet.ncbs.res.in/people-search?%s&page=%s" %( query , page )
    req = urllib2.Request( url )
    response = urllib2.urlopen( req )
    text =  response.read( ) 
    return BeautifulSoup( text, 'html.parser' )

def data( text ):
    m = re.search( '\[(.+)\]', text )
    if m:
        return m.group(1).strip()
    else:
        return text.strip()

def toLine( text ):
    text = text.replace( ',', ' ' )
    text = text.replace( '\n', ' ' )
    res = [ data(x) for x in  filter(None, text.split( '|' )) ]
    res = filter( None, res )
    res = [ x.encode( 'utf8', 'ignore' ).strip( ) for x in res ]
    return ",".join(res[:-2])

def findPeople( soup ):
    results = []
    for elem in soup.findAll( 'tr' ):
        if u'mailto' in unicode(elem):
            try:
                people = toLine( html2text.html2text( str( unicode(elem) ) ) )
                results.append( people )
            except Exception as e:
                print( e )
                pass

    return results


def main( query ):
    i = 0
    people = findPeople( queryIntranet( query, 0 ) )
    print( "\n".join( people ) )
    while len(people) >= 20:
        i += 1
        people = findPeople( queryIntranet( query, i ) )
        print( "\n".join( people ) )
        if i == 100:
            break

    people = findPeople( queryIntranet( query, i+1 ) )
    print( "\n".join( people ) )

if __name__ == '__main__':
    query = 'name='+ sys.argv[1]
    main( query )
