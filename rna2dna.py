#!/usr/bin/env python
import sys
import os
import read_fasta

def main():
    rnaFile = sys.argv[1]
    print("[INFO] Got rna file %s" % rnaFile)
    seqs = read_fasta.read_fasta( rnaFile )
    
    dnaFile = '%s_2dna.fa' % rnaFile
    with open(dnaFile, 'w') as f:
        for s in seqs:
            f.write('>%s\n%s\n' % (s, seqs[s].replace('U', 'T')))
    print("[INFO] RNA -> DNA. Written to %s" % dnaFile)
    

if __name__ == "__main__":
    main()
