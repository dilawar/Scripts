from collections import defaultdict 

def read_fasta( filename, unique = False ):
    """Read sequences from a fasta file.
    Return a defaultdict 
    """
    print("[INFO] Reading %s, Are collecting only unique: %s" % (filename, unique))
    if unique == True:
        seqsDict = defaultdict( set )
    else:
        seqsDict = defaultdict(list)
    with open( filename, "r") as f:
        text = f.read()
    blocks = text.split('>')
    for b in blocks:
        data = filter(None, b.split('\n'))
        if data:
            header, seq = data[0], data[1:]
            if unique == True:
                seqsDict[header].add("".join(seq))
            else:
                seqsDict[header].append("".join(seq))
    return seqsDict

def read_text( filename, unique = False ):
    print("[INFO] Reading %s, Are collecting only unique: %s" % (filename, unique))
    if unique == True:
        seqsDict = defaultdict( set )
    else:
        seqsDict = defaultdict(list)
    with open( filename, "r") as f:
        text = f.read()
    lines = text.split('\n')
    for l in lines:
        l = l.strip()
        if not l:
            continue
        header, seq = l, l
        if unique == True:
            seq[header].add(seq)
        else:
            seqsDict[header].append(seq)
    return seqsDict
