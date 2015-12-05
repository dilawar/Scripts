#!/usr/bin/env python 

"""
    notepal.py
    Author : Dilawar Singh
    Institute : IIT Bombay
    Email : dilawar@ee.iitb.ac.in
    Log : Created on Feb 16, 2012

    ABOUT : This module fetch assignements from moodle course page as specified
    in its configuration file .moodlerc which must be located in user home
    folder. See this file for options.

                                       
"""

import os
import re
import mechanize
import html2text
import cookielib
import sys, os, shutil, getpass, glob, subprocess
from bs4 import BeautifulSoup
import urllib2 as urllib

class Notepal():

    """ A python application to access moodle and download data from it.
    """
    def __init__(self):
        print("Initializing notepal ... ")
        self.url = "https://doqcs.ncbs.res.in/notepal2015"
        self.proxy = None
        self.br = mechanize.Browser( factory=mechanize.RobustFactory())
        cj = cookielib.LWPCookieJar()
        self.br.set_cookiejar(cj)
        self.br.set_handle_equiv(False)
        self.br.set_handle_robots(False)
        self.br.set_handle_referer(False)
        self.br.set_handle_redirect(True)
        self.br.set_debug_redirects(True)
        self.br.set_debug_responses(False)
        self.br.set_debug_http(False)
        self.br.set_handle_refresh(mechanize._http.HTTPRefreshProcessor(), max_time=2)
        self.br.addheaders = [('User-agent', 'Mozilla/5.0 (X11; U; Linux 1686; en-US;\
            rv:1.9.0.1) Gecko/201171615 Ubuntu/11.10-1 Firefox/3.0.1')]
    
    def set_proxy(self, proxy=None):
        if not proxy:
            self.br.set_proxies({})
        else:
            self.br.set_proxies({"http": os.environ['http_proxy']
                , "ftp": os.environ['ftp_proxy']
                , "https" : os.environ['https_proxy']}
                )

    def make_connection(self, username, password=None):
        if self.proxy != "false" :
            print("Using proxy variables from environment ...")
        else :
            print("Ignoring proxy variables...")
            self.set_proxy()

        print("Logging into NOTEPAL : %s" % self.url)
        try:
            res = self.br.open(self.url)
        except Exception as e:
            pass
        # select the form and login
        assert self.br.viewing_html()
        form_id = 0;
        for i in self.br.forms():
            fid = i.attrs.get('id') 
            fid = fid.lower()
            if "login" in fid:
                self.br.select_form(nr = form_id)
                self.br.form['name'] = username.strip()
                if os.environ.get('NOTEPAL_PASSWORD', None):
                    password = os.environ['NOTEPAL_PASSWORD']
                else:
                    password = getpass.getpass("Your notepal password please:")
                self.br.form['pass'] = password
                res = self.br.submit()
                res = self.br.response()
                if res:
                    print("Login successful")
                continue
            else:
                form_id = form_id + 1;

    def start(self, args):
        print("[INFO] Doing thingy")
        if 'list' in args:
            print("[INFO] Getting list of all notes")
            res = self.br.follow_link( text_regex = 'Recent posts')
            baseurl = self.br.geturl()
            for i, l in enumerate(self.br.links( url_regex = 'content\/' )):
                print("%3s: %s" % (i, l.text))
        else:
            print("[INFO] Unsupported args: %s" % args.keys())

def main(args):
    notepal = Notepal()
    notepal.make_connection( username = 'dilawars' )
    notepal.start( args )
        
if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''description'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('--new', '-n'
        , required = False
        , help = 'Write a new note'
        )
    parser.add_argument('--list', '-l'
        , required = False
        , help = 'List all notes.'
        )
    parser.add_argument('--summary', '-s'
        , required = False
        , help = 'Summary of lab notes'
        )
    parser.add_argument('--username', '-u'
        , required = True
        , help = 'Username'
        )
    class Args: pass 
    args = Args()
    parser.parse_args(namespace=args)
    main( vars(args) )
