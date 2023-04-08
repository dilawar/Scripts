#!/usr/bin/env python

import typer
import ftplib
import urllib.parse
import os
import typing as T

app = typer.Typer()

def _parse_ftp_uri(uri):
    uri = uri.replace("ftp://", "")
    uri = uri.split('@', maxsplit=2)
    upass = uri[0]
    if ':' not in upass:
        upass += ':';
    username, password, url = *upass.split(':'), uri[-1]
    return dict(username=username, password=password, host=url)

URI_ = None
URI_ = _parse_ftp_uri(os.environ.get('FTP_URI', ''))

def get_from_env(env: str, uriattr:str):
    v = os.environ.get(env, '')
    if not v and URI_ is not None:
        v = getattr(URI_, uriattr)
    assert v, f"Could not determine {uriattr}"
    return v

def get_user() -> T.Optional[str]:
    return get_from_env('FTP_USER', 'username')

def get_password()->T.Optional[str]:
    return get_from_env('FTP_PASSWORD', 'password')

def get_url() ->  T.Optional[str]:
    return get_from_env('FTP_HOST', 'host')


def get_creds(url, user, password):
    url = url or get_url()
    user = user or get_user()
    password = password or get_password()
    return url, user, password


class FTP(ftplib.FTP):
    def __init__(self, url:str = '', user:str='', password:str=''):
        url, user, password = get_creds(url, user, password)
        try:
            super().__init__(url, user, password)
        except Exception as e:
            print(f"Failed to login: {e}")
            quit(1)


@app.command()
def ls(url:str='', user:str='', password: str=''):
    ftp = FTP(url, user, password)
    print(ftp.retrlines('LIST'))

@app.command()
def upload(url, pat:str):
    print(f"Downloading using pattern {pat}")


if __name__ == "__main__":
    app()


