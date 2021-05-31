#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np
import cv2


def main(args):
    imgs = [cv2.imread(f) for f in args.FILES]

    # if args.vertical:
    #     allImgs = np.vstack(imgs)
    # else:
    #     allImgs = np.hstack(imgs)
    N = int(len(imgs) ** 0.5)
    for i, img in enumerate(imgs):
        ax = plt.subplot(N+1, N+1, i+1)
        ax.imshow(img, interpolation='none', aspect='auto')
        ax.axis(False)

    # print(f'[INFO] Stacking total of {len(allImgs)} images')
    # plt.imshow(allImgs, interpolation="none", aspect="auto")
    # plt.axis(False)

    if args.output is None:
        plt.show()
        return
    plt.savefig(args.output)
    print(f'[INFO] Saved to {args.output}')


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("FILES", nargs="+", help="Images")
    parser.add_argument(
        "--vertical", "-v", action="store_true", help="stack images horizontal"
    )
    parser.add_argument("--output", "-o", default=None, help="Save to this file. If `None`, display the image.")
    args = parser.parse_args()
    main(args)
