#!/usr/bin/env python

#
# Wget like interface using python.
#

import typer
import typing as T
import urllib.request
import urllib.parse

def main(url: str, output: T.Optional[str] = None):

    r = urllib.parse.urlparse(url)
    assert r.scheme and r.netloc, f"{url} is not valid?"

    if output is None:
        output = url.split('/')[-1]
    typer.echo(f"Downloading {url} -> {output}")
    r = urllib.request.urlretrieve(url, output)
    assert r

if __name__ == "__main__":
    typer.run(main)
