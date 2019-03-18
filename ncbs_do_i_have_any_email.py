#!/usr/bin/env python3

import imaplib
import getpass

url_ = 'imap.ncbs.res.in'

def main():
    with imaplib.IMAP4(url_) as M:
        M.noop()
        password = getpass.getpass()
        M.login('dilawars', password)
        M.select()
        #  typ, data = M.search(None, 'ALL')
        typ, data = M.search(None, '(UNSEEN)')
        print('Total new msg: %d' % len(data[0].split()))

        # FETCH?
        #for num in data[0].split():
        #    typ, data = M.fetch(num, '(SUBJECT FROM)')
        #    print('Message %s\n%s\n' % (num, data[0][1]))
        #    typ, data = M.store(num, '-FLAGS', '\\Seen')

        M.close()
        M.logout()

if __name__ == '__main__':
    main()
