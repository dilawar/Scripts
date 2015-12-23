#!/usr/bin/env python
# This  script removes animal RNA from a given fasta file.
import sys
import os

seqs_ = {}
plants_ = {}

accepted_ = {}
rejected_ = {}

script_path = os.path.dirname( __file__ )
config_file = os.path.join( script_path, 'configs/plant_organisms.txt' )

def parse_config():
    with open(config_file, "r") as f:
        config = f.read().split('\n')

    for l in config:
        if l:
            l = l.split('\t')
            plants_[l[0]] = l[1:]
    print("[INFO] Generated the dict having acceptable plants")

def remove_animal( fasta_file ):
    print("[INFO] Processing %s" % fasta_file)
    with open( fasta_file, "r") as f:
        text = f.read()
    blocks = text.split('>')
    for b in blocks:
        data = filter(None, b.split('\n'))
        if data:
            header, seq = data[0], data[1:]
            seqs_[header] = "".join(seq)
    for k in seqs_:
        pId = k.split('-')[0]
        if pId in plants_:
            accepted_[k] = seqs_[k]
        else:
            rejected_[k] = seqs_[k]
    print("[INFO] Total %s plants, %s animals" % (len(accepted_),
        len(rejected_)))

    
    outfile = "%s_animals.fa" % fasta_file
    with open(outfile, 'w') as f:
        for a in rejected_:
            f.write(">%s\n%s\n" % (a, rejected_[a]))
    print('[INFO] Wrote animals miRNA to %s' % outfile)

    outfile = "%s_plants.fa" % fasta_file
    with open(outfile, 'w') as f:
        for a in accepted_:
            f.write(">%s\n%s\n" % (a, accepted_[a]))
    print('[INFO] Wrote plants miRNA to %s' % outfile)



def main():
    fastaFile = sys.argv[1]
    parse_config()
    remove_animal( fastaFile )

if __name__ == "__main__":
    main()

