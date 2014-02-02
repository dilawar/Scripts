#!/usr/bin/env python  
import re
import os 
import sys

pat = re.compile(r'\<{5,7}\s+HEAD\n(.*?)\={5,7}\n(.*?)\>{5,7}\s\w{39,40}'
        , re.IGNORECASE | re.DOTALL
        )

def selectHead(txt):
    global pat
    txt = pat.sub(r'\1', txt)
    return txt

def selectRev(txt):
    global pat 
    txt = pat.sub(r'\2', txt)
    return txt

if __name__ == "__main__":
    filename = sys.argv[1]
    if not os.path.isfile(filename):
        print("Path does not exists")
        sys.exit(0)
    txt = open(filename, "r").read()
    newTxt = selectHead(txt)
    with open(filename, "w") as f:
        f.write(newTxt)


