#!/usr/bin/env python3

__author__ = "Dilawar Singh"
__email__ = "dilawar@subcom.tech"

from PIL import Image, ImageDraw, ImageFont

args_ = None


def main():
    global args_
    W = int(args_.size)
    font_scale = W
    img = Image.new("L", (W, W), 10)
    draw = ImageDraw.Draw(img)
    FONT = ImageFont.truetype(args_.font, font_scale)
    FONT_COLOR = 255
    T = args_.LETTER
    draw.text((W // 5, 0), f"{T}", font=FONT, fill=FONT_COLOR)
    img.save(f"{T}.png")


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser()
    parser.add_argument("LETTER", help="Letter to convert")
    parser.add_argument(
        "--size", "-s", default=10, type=int, help="Image size will be w×w"
    )
    parser.add_argument(
        "--pen-width",
        "-p",
        default=0,
        type=int,
        help="Pen width in pixel (default: compute automatically)",
    )
    parser.add_argument(
        "--font",
        "-F",
        default="DejaVuSansMono.ttf",
        help="Which font to use. Default DejaVuSansMono.ttf",
    )
    args_ = parser.parse_args()
    main()
