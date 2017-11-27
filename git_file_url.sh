#!/bin/bash
if [ ! $# -eq 1 ]; then
    echo "USAGE: $0 file_in_repo"
    exit
fi

function sshToHttp 
{
python <<END
import re
url = "$1"
m = re.search(r"git\@github\.com\:(?P<name>[\w/]+)", url)
if m:
    print("http://github.com/"+m.group('name')+"/raw/master")
else:
    print(url+"/raw/master")
END
}
filename=`basename $1`
dir=`dirname $1`
( 
    cd $dir
    repo_url=`git config --get remote.origin.url`
    repo_url=`sshToHttp $repo_url`
    file_name=`git ls-files --full-name -- $filename`
    echo $repo_url/$file_name
)
