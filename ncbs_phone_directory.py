#!/usr/bin/env python3

import requests
import sys
import lxml.html

url_ = 'https://intranet.ncbs.res.in/information/telephone'

def main(query):
    global url_
    r = requests.get(url_)
    et = lxml.html.document_fromstring(r.text)
    tables = et.xpath( '//table')
    for table in tables:
        for tr in table.xpath( '//tr'):
            tds = [x.text.encode('ascii', 'ignore') for x in tr.xpath('./td') if x.text is not None]
            if len(tds) < 4:
                continue
            # printable
            tds = [x.decode() for x in tds]
            line = '\t'.join(tds)
            if query.lower() in line.lower():
                print(line)

if __name__ == '__main__':
    query = sys.argv[1]
    main(query)
