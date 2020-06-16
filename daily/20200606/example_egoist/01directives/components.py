from __future__ import annotations
import typing as t
import typing_extensions as tx
from egoist.app import create_subapp

if t.TYPE_CHECKING:
    from requests import Session

app = create_subapp()
app.include("egoist.directives.shared")


@app.shared
def get_http_client() -> Session:
    import requests

    return requests.Session()


class Opener(tx.Protocol):
    def open(self, file: str, mode: str = ...) -> t.IO[t.Any]:
        ...


@app.shared
def get_file_opener() -> Opener:
    class _Opener:
        open = staticmethod(open)

    return _Opener
