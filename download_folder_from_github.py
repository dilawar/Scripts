#!/usr/bin/env python

"""download_folder_from_github.py: 

Last modified: Fri Jun 27, 2014  07:04PM

"""
    
__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2013, Dilawar Singh"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"


import os 
import sys
import urllib
import re
import urlparse
from HTMLParser import HTMLParser



class GithubHTMLParser(HTMLParser):
    
    def __init__(self):
        HTMLParser.__init__(self)
        self.dirs  = []

    def handle_starttag(self, tag, attrs):
        attrs = dict(attrs)
        if tag == 'a' and attrs.get('class', '') == 'js-directory-link':
            self.dirs.append(attrs)

class GitHub():
    def __init__(self, url):
        self.baseUrl = url
        self.basePath = urlparse.urlparse(url).path
        self.folders = []
        self.files = []
        self.parser = GithubHTMLParser()

    def listOfFolders(self, url=None):
        self.parser.dirs = []
        if not url:
            url = self.baseUrl
        else:
            url = "https://www.github.com{}".format(url)
        print("Getting list of folders: {}".format(url))
        htmltext = urllib.urlopen(url).read()
        self.parser.feed(htmltext)
        return self.parser.dirs

    def downloadAndSaveFile(self, urlFile):
        """Download and store the file """
        urlPath = urlparse.urlparse(urlFile).path
        filePath = urlPath.replace(self.basePath, '')
        print("== File path is {}".format(filePath))

    def downloadFolders(self, url=None):
        """Download folders in given link """
        dirs = self.listOfFolders(url)
        print("++ Total {} files and folders".format(len(dirs)))
        if not dirs:
            print("Looks like downloadable file {}".format(url))
            self.downloadAndSaveFile(url)
        else:
            self.folders += dirs
            while len(self.folders) > 0:
                dir = self.folders.pop(0)
                url = dir.get('href', None)
                if url:
                    self.downloadFolders(url) 

def main():
    if len(sys.argv) < 2:
        print("usage: {} url_to_github_folder".format(sys.argv[0]))
        sys.exit()
    url = sys.argv[1]
    github = GitHub(url)

    github.downloadFolders()

if __name__ == '__main__':
    main()
