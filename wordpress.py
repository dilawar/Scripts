#!/usr/bin/env python 

import wordpress_xmlrpc as wp
import wordpress_xmlrpc.methods.posts as wpposts
import argparse

parser = argparse.ArgumentParser(description="Your wordpress client.")
parser.add_argument('--url', metavar='url', required=True
  , help="Your wordpress url. Append xmlrpc.xml")
parser.add_argument('--user', metavar='u', required=True
    , help="Username")
parser.add_argument('--password', metavar='p', required=True 
    , help="Password")
parser.add_argument('--proxy', metavar='proxy'
    , default=None
    , help="Proxy if required")

wpargs = parser.parse_args()

url = wpargs.url
user = wpargs.user
password = wpargs.password
wpClient = wp.Client(url, user, password)
posts = wpClient.call(wpposts.GetPosts())
for p in posts :
  print p.title


