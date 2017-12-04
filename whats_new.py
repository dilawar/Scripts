#!/usr/bin/env python3
"""whats_new.py: 

Search the following services for latest papers:


"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import sys
import os
import lxml.html

if sys.version_info.major == 2:
    import urllib2 as urllib
else:
    import urllib.request as urllib

services_ = { 
        'pubmed' :  'https://www.ncbi.nlm.nih.gov/pubmed/'
        , 'biorxiv' : 'https://www.biorxiv.org/search/'
        , 'bioarchive' : 'https://www.biorxiv.org/search/'
        }

args_ = None

def _make_query( query ):
    return query

def _make_url( query, service = 'pubmed' ):
    global services_
    baseurl = services_.get( service, '' )
    if not baseurl:
        print( '[WARN] Service %s is not configured' % service )
        return None

    url = baseurl
    if 'pubmed' == service:
        url = '%s/?term=%s' % (baseurl, query)

    return url


def main( args ):
    args_ = args
    url = _make_url( args.query, 'pubmed' )
    res = urllib.urlopen( url ).read( )
    etree = lxml.html.fromstring( res )
    links = etree.xpath( '//div[@class="rslt"]//a' )
    for i, l in enumerate( links ):
        print( "%d : %s" % (i, l.text) )


if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''What's new.'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--query', '-q'
        , required = True
        , help = 'Query phrase. e.g. "Bum Chick Bow Bow,author:Chuck Norris" '
                ' or something more scientific.'
        )
    parser.add_argument('--output', '-o'
        , required = False, default = 'sys.stdout'
        , help = 'Output file'
        )
    parser.add_argument('--subject', '-s'
        , required = False, default = 'biology'
        , help = 'Subject wise search.Not in very good shape'
        )
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main( args )

