#!/bin/bash
getmail=getmail
echo "Setting up getmail"

if [ ! `which getmail` ]; then
    if [ ! `which getmail4` ]; then
        echo "No getmail program found."
        exit
    else
        getmail=$(which getmail4)
    fi
else
    getmail=$(which getmail)
fi

if [ ! -d $HOME/.getmail ]; then
    mkdir -p $HOME/.getmail
fi

if [ ! -d $HOME/Mail ]; then
    mkdir -p $HOME/Mail/GPO/{cur,new,temp}
    mkdir -p $HOME/Mail/EE/{cur,new,temp}
    mkdir -p $HOME/Mail/NCBS/{cur,new,temp}
    mkdir -p $HOME/Mail/GMAIL/{cur,new,temp}
    touch $HOME/Mail/{GPO,EE,NCBS,GMAIL}/backup.mbox
fi

if [ ! -f $HOME/.getmail/ee ]; then
cat > $HOME/.getmail/ee <<EOL
[retriever]
type = SimpleIMAPRetriever
server = sandesh.ee.iitb.ac.in
username = profchaos
password = ihatekartman
 
[destination]
type = MultiDestination
destinations = ('[mboxrd-destination]', '[maildir-destination]')
 
[mboxrd-destination]
type = Mboxrd
path = $HOME/Mail/EE/backup.mbox
 
[maildir-destination]
type = Maildir
path = $HOME/Mail/EE/
 
[options]
verbose = 2
read_all = False
message_log = $HOME/.getmail/ee.log 
EOL
fi

if [ ! -f $HOME/.getmail/gpo ]; then
cat > $HOME/.getmail/gpo <<EOL
[retriever]
type = SimpleIMAPRetriever
server = imap.iitb.ac.in
username = profchaos
password = ihatekartman
 
[destination]
type = MultiDestination
destinations = ('[mboxrd-destination]', '[maildir-destination]')
 
[mboxrd-destination]
type = Mboxrd
path = $HOME/Mail/GPO/backup.mbox
 
[maildir-destination]
type = Maildir
path = $HOME/Mail/GPO/
 
[options]
verbose = 2
read_all = False
message_log = $HOME/.getmail/gpo.log 
EOL
fi

if [ ! -f $HOME/.getmail/gmail ]; then
cat > $HOME/.getmail/gmail <<EOL
[retriever]
type = SimpleIMAPRetriever
server = imap.gmail.com
username = profchaos
password = ihatekartman
 
[destination]
type = MultiDestination
destinations = ('[mboxrd-destination]', '[maildir-destination]')
 
[mboxrd-destination]
type = Mboxrd
path = $HOME/Mail/GMAIL/backup.mbox
 
[maildir-destination]
type = Maildir
path = $HOME/Mail/GMAIL/
 
[options]
verbose = 2
read_all = False
message_log = $HOME/.getmail/gmail.log 
EOL
fi

if [ ! -f $HOME/.getmail/ncbs ]; then
cat > $HOME/.getmail/ncbs <<EOL
[retriever]
type = SimpleIMAPRetriever
server = imap.ncbs.res.in
username = profchaos
password = ihatekartman
 
[destination]
type = MultiDestination
destinations = ('[mboxrd-destination]', '[maildir-destination]')
 
[mboxrd-destination]
type = Mboxrd
path = $HOME/Mail/NCBS/backup.mbox
 
[maildir-destination]
type = Maildir
path = $HOME/Mail/NCBS/
 
[options]
verbose = 2
read_all = False
message_log = $HOME/.getmail/ncbs.log 
EOL
fi

echo "Edit files manually in $HOME/.getmail"
