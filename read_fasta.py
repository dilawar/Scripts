from collections import defaultdict 

def read_fasta( filename, unique = False ):
    print("[INFO] Reading %s, Are collecting only unique: %s" % (filename, unique))
    if unique == True:
        seqs = defaultdict( set )
    else:
        seqs = defaultdict(list)
    with open( filename, "r") as f:
        text = f.read()
    blocks = text.split('>')
    for b in blocks:
        data = filter(None, b.split('\n'))
        if data:
            header, seq = data[0], data[1:]
            if unique == True:
                seq[header].add("".join(seq))
            else:
                seqs[header].append("".join(seq))
    return seqs

