#!/usr/bin/env python3

import wave
import sys
import struct
import scipy.signal
import numpy as np
import argparse

parser = argparse.ArgumentParser()
parser.add_argument("FILE", help="Input wav file")
parser.add_argument("OUTFILE", help="Output wav file")
parser.add_argument("-t", "--threshold", default=0.25, help="std of envelop")
parser.add_argument("-p", "--plot", default="", help="plot to this file (png)")
args = parser.parse_args()


def smooth(y, N=64):
    win = np.ones(N) / N
    return np.convolve(y, win, "same")


infile = args.FILE
data = []

with wave.open(infile, "rb") as f:
    sr = f.getframerate()
    sw = f.getsampwidth()
    N = f.getnframes()
    nch = f.getnchannels()
    assert nch == 1, "Only for mono-channel files"
    frames = f.readframes(N)
    print(f"[INFO] sr={sr} per sec, channels={nch}")
    data = struct.unpack("h" * int(len(frames) / sw / nch), frames)
    if sw == 2:
        data = np.int16(data)
    else:
        raise RuntimeError(f"sample size {sw} is not supported")

env = np.abs(data)
env = smooth(env)

s = env.std()
print(f" std={s}")
thres = args.threshold * s

assert len(env) == len(data)

start, blocks = False, []
block = [0, 0]
for i, e in enumerate(env):
    if e >= thres:
        start = True
        block[0] = i
    elif start and e < thres:
        start = False
        block[1] = i
        blocks.append(block)
        block = [0, 0]

A, B = blocks[0][0], blocks[-1][1]

print(f"[INFO] Pruning between {A/sr}s and {B/sr}s")
newdata = data[A:B]

with wave.open(args.OUTFILE, "wb") as f:
    assert nch == 1, "Only for mono-channel files"
    f.setnchannels(nch)
    f.setsampwidth(2)
    f.setframerate(sr)
    f.setnframes(len(newdata) * 2)
    f.writeframesraw(newdata.tobytes())

if args.plot:
    import matplotlib.pyplot as plt

    tvec = np.linspace(0, len(data) / sr, len(data))
    plt.subplot(211)
    plt.plot(tvec, data, label="raw data")
    plt.plot(tvec, env, label="envelop")
    plt.axhline(thres, label="thres")
    for block in blocks:
        print(block)
        plt.axvline(block[0] / sr, color="red")
        plt.axvline(block[1] / sr, color="blue")
    plt.legend()
    plt.subplot(212)
    newT = np.linspace(A / sr, B / sr, B - A)
    plt.plot(newT, newdata)

    outfile = args.plot
    plt.savefig(outfile)
    print(f"Debug plot is saved to {outfile}")
