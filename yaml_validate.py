#!/usr/bin/env python3

__author__           = "Dilawar Singh"
__email__            = "dilawar.s.rajput@gmail.com"

import yaml
import sys
from pathlib import Path

def main():
    infile = Path(sys.argv[1])
    with infile.open('r') as f:
        try:
            data = yaml.load(f.read())
        except Exception as e:
            print(e)
            quit(-1)
        print(data)

if __name__ == "__main__":
    main()
