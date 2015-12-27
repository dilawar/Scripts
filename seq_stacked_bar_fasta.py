import pandas as pd
import numpy as np
import scipy
import sys
import os
from Bio import SeqIO, SeqUtils
from collections import defaultdict
import matplotlib.pyplot as plt
from pandas import read_csv
from collections import Counter
#plt.style.use('ggplot')
import read_fasta
listoflist = []

def ReadSeparater(sequencefile):
	global sequences_
    sequences_ = read_fasta.read_fasta(sequencefile)
    for seq in sequences_:
    	if 
    with open(sequencefile, "r") as seqF:
        sequences = seqF.readlines()
    countGC(sequences, outfile)
    plotGCBar(outfile)


def countGC(sequences, outfile):
    for s in sequences:
        listoflist.append(list(s.rstrip()))
    char_matrix = np.matrix(listoflist)

    ## Iterating over the column of matrix.
    data = []
    for col in char_matrix.T:
        count = {}
        col = col.getA1()
        a = Counter(col)
        for val in a:
            count[val] = float(a[val]) / len(col)
        data.append(count)
    savecount(data, outfile)

def savecount(data, outfile):
    print("Writing count to file %s" % outfile)
    header = [ "G", "C", "A", "U"]
    keys = [ "G", "C", "A", "T"]
    with open(outfile, "w") as f:
        f.write(",".join(header))
        f.write("\n")
        for count in data:
            line = []
            for key in keys:
                line.append(str(count.get(key, 0)))
            f.write("{}\n".format(",".join(line)))
            
def plotGCBar(outfile):
    #frame = pd.DataFrame()
    data = pd.read_csv(outfile)
    seq = range(1,25)
    title = outfile.split('/')
    #title = filename[-1].split('.')
    plt.figure(facecolor="white")
    my_plot = data.plot(kind='bar', stacked=True, color=['#FF9696','#FF6262','#85E085','#47D147'] , rot = 0, grid=False)
    #my_plot.set_title("%s"%title[-1].strip('.csv.txt'), fontsize=15)
    my_plot.set_title("Arabidopsis thaliana miRNA mature sequences")
    my_plot.set_xticklabels(seq, fontsize=14)
    my_plot.set_xlabel("Position", fontsize=14)
    my_plot.set_ylabel("Percentage", fontsize=14)
    my_plot.legend(loc=9, ncol=4)
    my_plot.axhline(0.5, color='grey',linewidth=1, ls = '-.')
    #plt.show()
    outfile = '%s_GC.png' % outfile
    print("Saving figures to %s" % outfile)
    plt.savefig(outfile, transparent=True)
    plt.show()
    
def main(sequencefile, outfile):
    ReadSeparater(sequencefile)
    
    
    

if __name__ == '__main__':
    sequencefile = sys.argv[1]
    outfile = None
    if len(sys.argv) > 2:
        outfile = sys.argv[2]
    main(sequencefile, outfile)
