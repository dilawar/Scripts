#!/usr/bin/env python
import re
def is_canonical(version):
    return re.match(r'^([1-9]\d*!)?(0|[1-9]\d*)(\.(0|[1-9]\d*))*((a|b|rc)(0|[1-9]\d*))?(\.post(0|[1-9]\d*))?(\.dev(0|[1-9]\d*))?$', version) is not None

def main( ):
    return is_canonical( sys.argv[1] )

main( )
