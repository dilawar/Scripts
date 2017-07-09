#!/usr/bin/env python
#
# Copyright 2013 by Martin Bley <martin@mb-oss.de> 
#
# maic.py is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# maic.py is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with maic.py. If not, see <http://www.gnu.org/licenses/>.
#

import time, sys, os, stat, ConfigParser, imaplib, re

class ImapChecker(object):
    
    Error = imaplib.IMAP4.error
    AbortError = imaplib.IMAP4.abort
    ReadOnlyError = imaplib.IMAP4.readonly
    version = "0.2"
    
    def __init__(self, host, ssl=False, port=None):
        
        self.host = host
        self.ssl = ssl
        self.port = port
        
        if self.port is None:
            if self.ssl is True:
                self.port = 993
            else:
                self.port = 143

    def login(self, user, passwd):
        if self.ssl is True:
            self.conn = imaplib.IMAP4_SSL(self.host, self.port)
        else:
            self.conn = imaplib.IMAP4(self.host, self.port)
    
        m = self.conn

        user = bytes(user)
        passwd = bytes(passwd)
        
        m.login( user, passwd )
        
    def logout(self):
        self.conn.logout()

    def check(self, folder="INBOX"):
        (rtn, msg) = self.conn.status(folder, "(MESSAGES RECENT UNSEEN)")

        re_status = re.compile(r'.*\((?P<status_items>.*)\).*$')

        if rtn == 'OK':
            match = re_status.search(str(msg[0]))
            
            if not match:
                raise self.Error('Could not get the folder status')

            status_items = match.group('status_items').strip().split()
            
            out = {}
            while status_items:
                key = status_items.pop(0)
                value = status_items.pop(0)
                out[key] = value
            return out
        
        raise self.Error('Status command failed')

    def shutdown(self):
        self.conn.shutdown()


def main():
    version = "0.4"
    options_required = ['servername', 'username', 'password']
    outstring = ""

    # config file 
    configfile = os.path.expanduser('~/.config/awesome/ImapWidget.cfg')
    config = ConfigParser.ConfigParser()

    # read config
    try:
        config.read(configfile)
        mode = os.stat(configfile).st_mode
        uid = os.stat(configfile).st_uid
    except (IOError, OSError):
        sys.stderr.write("Error opening config file " + str(configfile))
        sys.exit(1)
    else:
        # check and fix mode of the config file 
        if os.getuid() != uid: 
            sys.stderr.write("You are not the owner of the config file.")

        if oct(stat.S_IMODE(mode)) != '0600':
            sys.stderr.write("Mode is not OK. Fixing permissions.")
            os.chmod(configfile, 0600)

    # iterate through the sections of the config 
    configSections = config.sections()

    # check, if all needed parameters are given in configfile
    for sec in configSections:
        options = config.options(sec)
        for req in options_required:
            if not(req in options):
                sys.stderr.write("Section " + sec + ": Missing option " + req + ". Removing it from Accountlist")
                configSections.remove(sec)

    # create IMAP connection for each account
    for sec in configSections:
        try:
            ssl = config.get(sec, 'ssl')
            if ssl in [ "True", "1", "Yes"]:
                imap = ImapChecker(config.get(sec, 'servername'), True)
            else:
                imap = ImapChecker(config.get(sec, 'servername'))
        except ConfigParser.NoOptionError:
            imap = ImapChecker(config.get(sec, 'servername'))

        if outstring is not "":
            outstring += " | "

        try:
            imap.login(config.get(sec, 'username'), config.get(sec, 'password'))
            ret = imap.check()
            new = ret['UNSEEN']
            recent = ret['RECENT']
            
            if int(new) > 0:
                new = "<span color=\"red\">" + new + "</span>"
            if int(recent) > 0:
                recent = "<span color=\"red\">" + recent + "</span>"

            imap.logout()
            outstring += sec + ": " + new + "/" + recent
        except:
            outstring += sec + ": ~"


    print(outstring)
    sys.exit(0)

if __name__ == '__main__':
    main()

