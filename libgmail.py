#!/usr/bin/env python 
import imaplib
import socks 
import socket

socket.setdefaultproxy(socks.PROXY_TYPE_SOCKS4, 'http://netmon.iitb.ac.in', '80', True)
socket.socket = socks.socksocket

mail = imaplib.IMAP4_SSL('imap.gmail.com')
mail.login('dilawar.rajput@gmail.com', 'rashmirathikurukchetra')
mail.list()
