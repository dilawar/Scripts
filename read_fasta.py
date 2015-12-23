def read_fasta( filename ):
    print("[INFO] Reading fasta file %s" % filename)
    seqs = {}
    with open( filename, "r") as f:
        text = f.read()
    blocks = text.split('>')
    for b in blocks:
        data = filter(None, b.split('\n'))
        if data:
            header, seq = data[0], data[1:]
            seqs[header] = "".join(seq)
    return seqs

