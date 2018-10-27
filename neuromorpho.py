#!/usr/bin/env python3
"""
Scraper for querying NeuroMorpho.Org from Python >= 3.6

The current API http://www.neuromorpho.org/apiReference.html#introduction does
not have url to download SWC files.

For more on NeuroMorpho.Org, see:

    Ascoli GA, Donohue DE, Halavi M. (2007) NeuroMorpho.Org: a central
    resource for neuronal morphologies.J Neurosci., 27(35):9247-51

""" 

import re
import json
import base64
import requests
import logging
import mechanicalsoup

logging.basicConfig(level=logging.DEBUG,
        format='%(asctime)s %(name)-12s %(levelname)-8s %(message)s',
        datefmt='%m-%d %H:%M'
        )
logger = logging.getLogger('neuromopho')

_cache    = {}
api_base_ = 'http://neuromorpho.org/api'
args_     = None
base_url_ = 'http://neuromorpho.org/'
browser_  = mechanicalsoup.StatefulBrowser() #soup_config={'features': 'lxml'})

def _query_about_neuron(n):
    nid = n['neuron_id']
    print(nid )

def _query_about_neurons_api(nrns):
    [ _query_about_neuron(n) for n in nrns ]

def query_about_neurons_api( num_neurons ):
    global args_
    global api_base_
    url = api_base_ + f'/neuron?size={num_neurons}'
    logger.debug( url )
    r = requests.get( url )
    data = r.json()
    goodKey = 'neuronResources'
    for k, v in data.items():
        _query_about_neurons_api(v[goodKey]) if goodKey in v else None

def _parse_and_download( html ):
    tables = html.xpath( '//table' )
    for table in tables:
        print( etree.tostring(table) )

def download_swc_files(num):
    global browser_
    logger.info( f"Downloading {num} files" )
    url = base_url_ + '/byRandom.jsp' 
    browser_.open( url )
    s = browser_.get_current_page()
    print( 'type', type(s) )
    links = s.find_all( {'class':'screenshot'} )
    for l in links:
        print( l )

def main(args):
    args_ = args
    download_swc_files(args.num)

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''Download from neuromorpho database.'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--num', '-n'
        , required = False, default = 1, type=int
        , help = 'No of neurons to download. By default chosen randomly.'
        )
    parser.add_argument('--cell-type', '-t'
        , required = False
        , help = 'Cell Type'
        )
    parser.add_argument('--brain-region', '-b'
        , required = False 
        , help = 'Brain region.'
        )
    parser.add_argument('--species', '-s'
        , required = False
        , help = 'Species'
        )
    parser.add_argument('--category', '-c'
        , required = False
        , choices = { "amygdala", "blowfly", "Aspiny"}
        , help = 'Category'
        )
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main(args)
