"""find_faculty.py: 

    Search intranet for faculy.

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
    res = [ data(x) for x in  filter(None, text.split( '|' )) ]
    res = filter( None, res )
    return ",".join(res[:-2])

def findPeople( soup ):
    results = []
    for elem in soup.findAll( 'tr' ):
        if 'mailto' in str(elem):
            people = toLine( html2text.html2text( str(elem) ) )
            results.append( people )

    return "\n".join( results )


def main( query ):
    return findPeople( queryIntranet( query ) )
    

if __name__ == '__main__':
    query = 'name='+ sys.argv[1]
    print main( query )
