from imaplib import *
from socks import socksocket, PROXY_TYPE_SOCKS4, PROXY_TYPE_SOCKS5, PROXY_TYPE_HTTP

class SocksIMAP4(IMAP4):
    def __init__(self): pass 

    def open(self,host,port):
        self.host = host
        self.port = port
        self.sock = socksocket()
        self.sock.setproxy(PROXY_TYPE_HTTP,'netmon.iitb.ac.in'\
                , 80 \
                , 'dilawars'\
                , '%rashmirathi' \
                )
        IMAP4_SSL(host,port)

s = SocksIMAP4()
s.open('imaps://imap.gmail.com', '993')
