#!/usr/bin/env python3 
# -*- coding: utf-8 -*-

# NOTE: From https://www.dspguide.com/ch22/1.htm
# The perception of loudness relates roughly to the sound power to an exponent
# of 1/3. For example, if you increase the sound power by a factor of ten,
# listeners will report that the loudness has increased by a factor of about two
# (101/3 ≈ 2). This is a major problem for eliminating undesirable environmental
# sounds, for instance, the beefed-up stereo in the next door apartment. Suppose
# you diligently cover 99% of your wall with a perfect soundproof material,
# missing only 1% of the surface area due to doors, corners, vents, etc. Even
# though the sound power has been reduced to only 1% of its former value, the
# perceived loudness has only dropped to about 0.011/3 ≈ 0.2, or 20%.

__author__           = "Dilawar Singh"
__copyright__        = "Copyright 2017-, Dilawar Singh"
__version__          = "1.0.0"
__maintainer__       = "Dilawar Singh"
__email__            = "dilawars@ncbs.res.in"
__status__           = "Development"

import os
import scipy.signal
import numpy as np
import scipy.io.wavfile

class Args: pass 
args_ = Args()

def spectogram(wavfile: str):
    assert os.path.isfile(wavfile)
    sr, samples = scipy.io.wavfile.read(wavfile)
    samples = np.mean(samples, axis=1)
    print( f"[INFO ] Sampling rate {sr} Hz." )
    print( f"[INFO ] Number of samples {len(samples)}" )
    return scipy.signal.spectrogram(samples, fs=sr
            , scaling= 'spectrum'
            )

def main():
    global args_
    freqs, times, sgram = spectogram(args_.input)
    sgram = sgram ** (1/3.0)  # see the n note above.
    #  np.save(f"{args_.input}.npy", sgram)
    if args_.plot:
        import matplotlib.pyplot as plt
        plt.pcolormesh(times, freqs, sgram)
        plt.colorbar()
        plt.ylim( args_.low_freq, args_.high_freq)
        plt.ylabel( 'Frequency (Hz)')
        plt.xlabel( 'Time (sec)' )
        plt.title( r'$V^{2/3}$' )
        plt.tight_layout()
        plt.savefig( args_.plot )
        print( f"[INFO ] Saved to {args_.plot}" )
        

if __name__ == '__main__':
    import argparse
    # Argument parser.
    description = '''Play a wav file.'''
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('input', help = 'Input WAV file')
    parser.add_argument('--low-freq', '-L'
        , required = False, default = 10
        , help = 'Lower frequency'
        )
    parser.add_argument('--high-freq', '-H'
        , required = False, default = 5000
        , help = 'Higher frequency'
        )
    parser.add_argument('--plot', '-p'
        , required = False, type=str
        , help = 'Plot spectogram to this file'
        )
    parser.parse_args(namespace=args_)
    main()

