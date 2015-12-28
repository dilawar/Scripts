#!/usr/bin/env python
    
__author__           = "Anushree Narjala"
__copyright__        = "Copyright 2015, Anushree Narjala and NCBS Bangalore"
__credits__          = ["NCBS Bangalore"]
__license__          = "GNU GPL"
__version__          = "1.0.0"
__maintainer__       = "Anushree Narjala"
__email__            = "anushreen@ncbs.res.in"
__status__           = "Development"

import numpy as np
import scipy
import sys
import os
from Bio import SeqIO, SeqUtils
from collections import defaultdict
import matplotlib.pyplot as plt
try:
    from collections import Counter
except Exception as e:
    print('[INFO] On python 2.6')
    from backport_collections import Counter

#plt.style.use('ggplot')
import read_fasta
listoflist = []
sorted_seq_ = defaultdict(list)

def ReadSeparater(sequencefile):
    global sequences_
    global sorted_seq_ 

    sequences_ = read_fasta.read_fasta(sequencefile, unique = False)
from collections import defaultdict 

def read_fasta( filename ):
    print("[INFO] Reading fasta file %s" % filename)
    seqs = defaultdict(list)
    with open( filename, "r") as f:
        text = f.read()
    blocks = text.split('>')
    for b in blocks:
        data = filter(None, b.split('\n'))
        if data:
            header, seq = data[0], data[1:]
            seqs[header].append("".join(seq))
    return seqs


    for k in sequences_:
        for seq in sequences_[k]:
            sort_according_to_size(seq)

    # Once I have binned sequences according to their size, on each bin run
    # countGC
    for size in sorted_seq_:
        counts = count_gc( size, sorted_seq_[size] )
        outfile = '%s_size_%s.csv' % (sequencefile, size)
        savecount( counts, outfile )
        barplot_gc(size, outfile)

def sort_according_to_size( seq ):
    global sorted_seq_ 
    sorted_seq_[ len(seq) ].append(seq)

def count_gc(size, sequences):
    """Count GC content of columns"""
    print('[LOG] Counting nucleotide content in %s seqs (%s nuc long)' % (len(sequences), size ))
    data = np.matrix([ list(x) for x in sequences])
    # Inverse the matrix
    counts = []
    for col in data.T:
        counts.append(Counter(col.getA1()))
    return counts
    
def savecount(data, outfile):
    print("Writing nuc-count to file %s" % outfile)
    header = [ "G", "C", "A", "U"]
    keys = [ "G", "C", "A", "T"]
    with open(outfile, "w") as f:
        f.write(",".join(header))
        f.write("\n")
        for count in data:
            line = []
            for key in keys:
                line.append(str(count.get(key, 0)))
            f.write("{0}\n".format(",".join(line)))
    
def main(sequencefile, outfile):
    ReadSeparater(sequencefile)

if __name__ == '__main__':
    sequencefile = sys.argv[1]
    outfile = None
    if len(sys.argv) > 2:
        outfile = sys.argv[2]
    main(sequencefile, outfile)
