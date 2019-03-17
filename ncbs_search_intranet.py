#!/usr/bin/env python3
    
__author__           = "Me"
__copyright__        = "Copyright 2016, Me"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Me"
__email__            = ""
__status__           = "Development"

import sys
import requests
import lxml.html 
import html2text
import re

def queryIntranet(query, page = 0):
    url = f"https://intranet.ncbs.res.in/people-search?{query}&page={page}"
    req = requests.get(url)
    text =  req.text
    p = lxml.html.document_fromstring(text)
    return p

def data( text ):
    m = re.search( '\[(.+)\]', text )
    if m:
        return m.group(1).strip()
    else:
        return text.strip()

def toLine( text ):
    text = text.replace(',', ' ' )
    text = text.replace('\n', ' ' )
    res = [data(x) for x in  filter(None, text.split( '|' )) ]
    res = filter( None, res )
    res = [ x for x in res ]
    return ",".join(res[:-2])

def findPeople(html):
    results = []
    for elem in html.xpath('//tr'):
        text = lxml.html.tostring(elem)
        if b'mailto:' in text:
            people = toLine(html2text.html2text(text.decode()))
            results.append(people)
    return results

def main(query):
    i = 0
    people = findPeople(queryIntranet(query, 0))
    print("\n".join(people))
    while len(people) >= 20:
        i += 1
        people = findPeople(queryIntranet(query, i))
        print( "\n".join( people ) )
        if i == 100:
            break

    people = findPeople( queryIntranet( query, i+1 ) )
    print("\n".join(people))

if __name__ == '__main__':
    query = 'name='+ sys.argv[1]
    main(query)
