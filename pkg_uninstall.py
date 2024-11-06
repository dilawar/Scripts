#!/usr/bin/env python3

# run it with `sudo`

from pathlib import Path
import sys
import typing as T
import os
import subprocess


def main():
    pkgs = list_pkgs(sys.argv[1])
    for i, pkg in enumerate(pkgs):
        print(f"{i}: {pkg}")
    idx = input("Select a package to uninstall ")
    pkg = pkgs[int(idx)]
    print(f"Uninstalling {pkg}")
    uninstall(pkg)


def uninstall(pkg: str):
    to_remove = []
    for file in subprocess.run(
        ["pkgutil", "--only-files", "--files", pkg], text=True, capture_output=True
    ).stdout.split("\n"):
        if not file.strip():
            continue
        p = Path(f"/{file}")
        assert p.is_file(), f"{p} is not a file"
        to_remove.append((p, False))
    for d in subprocess.run(
        ["pkgutil", "--only-dir", "--files", pkg], text=True, capture_output=True
    ).stdout.split("\n"):
        if not d.strip():
            continue
        p = Path(f"/{d}")
        assert p.is_dir(), f"{p} is not a dir"
        to_remove.append((p, True))

    for path, is_dir in to_remove:
        if not is_dir:
            path.unlink()
        else:
            path.rmdir()

    subprocess.run(["sudo", "pkgutil", "--forget", pkg], text=True)


def list_pkgs(keyword) -> T.List[str]:
    pkgs = subprocess.run(
        ["pkgutil", "--pkgs"], text=True, capture_output=True
    ).stdout.split("\n")
    return [p for p in pkgs if keyword.lower() in p.lower()]


if __name__ == "__main__":
    main()
