#!/usr/bin/env python3

import typer
import datetime
import typing as T
from pathlib import Path


def main(title: str, postdir: T.Optional[Path] = None):
    today = datetime.datetime.now().strftime("%Y-%m-%d")
    year = datetime.datetime.now().strftime("%Y")
    if postdir is None:
        postdir = Path(".").parent / "_posts" / year
    else:
        postdir = Path(postdir)
    postdir.mkdir(parents=True, exist_ok=True)
    fname = postdir / f"{today}-{title.replace(' ', '-')}.md"
    assert not fname.exists(), f"{fname} already exists"
    with fname.open("w") as f:
        f.write("---\n")
        f.write(f"title: {title}\n")
        f.write(f"date: {today}\n")
        f.write(f"comments: true\n")
        f.write("---\n")
    print(f"Saved to {fname}")


if __name__ == "__main__":
    typer.run(main)
