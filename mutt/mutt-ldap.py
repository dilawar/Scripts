#!/usr/bin/env python
#
# Copyright (C) 2008-2011  W. Trevor King
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

"""LDAP address searches for Mutt.

Add :file:`mutt-ldap.py` to your ``PATH`` and add the following line
to your :file:`.muttrc`::

  set query_command = "mutt-ldap.py '%s'"

Search for addresses with `^t`, optionally after typing part of the
name.  Configure your connection by creating :file:`~/.mutt-ldap.py`
contaning something like::

  [connection]
  server = myserver.example.net
  basedn = ou=people,dc=example,dc=net

See the `CONFIG` options for other available settings.
"""

import email.utils
import itertools
import os.path
import ConfigParser

import ldap


CONFIG = ConfigParser.SafeConfigParser()
CONFIG.add_section('connection')
CONFIG.set('connection', 'server', 'ldap.iitb.ac.in')
CONFIG.set('connection', 'port', '389')  # set to 636 for default over SSL
CONFIG.set('connection', 'ssl', 'no')
CONFIG.set('connection', 'user', '')
CONFIG.set('connection', 'password', '')
CONFIG.set('connection', 'basedn', 'dc=iitb,dc=ac,dc=in')
CONFIG.read(os.path.expanduser('~/.mutt-ldap.rc'))

def connect():
    protocol = 'ldap'
    if CONFIG.getboolean('connection', 'ssl'):
        protocol = 'ldaps'
    url = '%s://%s:%s' % (
        protocol,
        CONFIG.get('connection', 'server'),
        CONFIG.get('connection', 'port'))
    connection = ldap.initialize(url)
    connection.bind(
        CONFIG.get('connection', 'user'),
        CONFIG.get('connection', 'password'),
        ldap.AUTH_SIMPLE)
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
        r = connection.search_s(
            CONFIG.get('connection', 'basedn'),
            ldap.SCOPE_SUBTREE,
            filterstr.encode('utf-8'))
    finally:
        if local_connection and connection:
            connection.unbind()
    return r

def format_entry(entry):
    cn,data = entry
    if 'mail' in data:
        for m in data['mail']:
            yield email.utils.formataddr((data['cn'][-1], m))


if __name__ == '__main__':
    import sys

    query = unicode(' '.join(sys.argv[1:]), 'utf-8')
    entries = search(query)
    addresses = list(itertools.chain(
            *[format_entry(e) for e in sorted(entries)]))
    print '%d addresses found:' % len(addresses)
    for i in addresses :
        print i
