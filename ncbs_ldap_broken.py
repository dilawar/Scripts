#!/usr/bin/env python
# Copyright (C) 2008-2011  W. Trevor King
# Copyright (C) 2013 - 2014      Dilawar Singh
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

""" Search NCBS LDAP server for information.

    Just pass whatever you want to search on NCBS LDAP as first arguement.
"""

import email.utils
import itertools
import os.path

try:
    import ConfigParser as cfg
except Exception:
    import configparser as cfg

import ldap3 as ldap

def connect():
    server = ldap.Server( 'ldaps://ldap.ncbs.res.in:8862'
            ,  use_ssl = True
            , get_info = ldap.ALL 
            )
    connection = ldap.Connection( server, auto_bind = True )
    return connection

def search(query, connection=None):
    local_connection = False
    try:
        if not connection:
            local_connection = True
            connection = connect()
        post = ''
        if query:
            post = '*'
        filterstr = '(|%s)' % (
            u' '.join([u'(%s=*%s%s)' % (field, query, post)
                       for field in ['cn', 'rdn', 'uid', 'mail']]))
        r = connection.search_ext_s(
            CONFIG.get('connection', 'basedn'),
            ldap.SCOPE_SUBTREE,
            filterstr.encode('utf-8')
            ,sizelimit = 2
            )
    finally:
        if local_connection and connection:
            connection.unbind()
    return r

def format_entry(entry):
    line = []
    cn, data = entry
    if data.get('givenName'):
        givenName = " ".join(data.get('givenName', []))
        name = " ".join(data.get('cn', []))
        line.append("{:10}: {} ".format("Name", givenName+" "+name))
        email = " ".join(data.get('mail', []))
        line.append("{:10}: {} ".format("Email", email))
        alternateEmail = " ".join(data.get('profileAlternateemail', []))
        line.append("{:10}: {} ".format("Email", alternateEmail))
        landline = data.get('profileLandline', [])
        line.append("{:10}: {} ".format("Landline",landline) )
        lab = " ".join(data.get('profileLaboffice', []))
        line.append("{:10}: {} ".format("Lab", lab))
        joinDate = " ".join(data.get('profileDateofjoin', []))
        line.append("{:10}: {} ".format("Date of join", joinDate ))
    elif data.get('macAddress', None):
        macId = " ".join(data['macAddress']).replace('-', ':')
        line.append("{:30}: {}".format("MacId", macId))
    return "\n".join(line)

if __name__ == '__main__':
    import sys
    if len(sys.argv) < 2:
        print("USAGE: {} query_words".format(sys.argv[0]))
        sys.exit(0)
    query =  ' '.join(sys.argv[1:])
    entries = search(query)
    # print entries
    addresses = [format_entry(e) for e in sorted(entries)]
    addresses = filter(lambda x: x != "", addresses)
    for add in addresses:
        print(".....................................")
        print(add)
