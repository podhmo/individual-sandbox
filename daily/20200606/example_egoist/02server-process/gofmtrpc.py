import typing as t
from egoist.app import App

NAME = __name__


def get_gofmtrpc_url() -> t.Optional[str]:
    from egoist.ext.serverprocess.runtime import get_discovery

    return get_discovery().lookup(NAME)


_id = 0


def gofmt(code: str) -> str:
    global _id
    from egoist.ext.serverprocess.runtime import get_http_client

    url = get_gofmtrpc_url()
    assert url is not None
    _id += 1
    res = get_http_client().post(
        url,
        json={
            "jsonrpc": "2.0",
            "id": _id,
            "method": "format",
            "params": {"code": code},
        },
    )
    res.raise_for_status()
    formatted = res.json()["result"]  # type:str
    return formatted


def includeme(app: App) -> None:
    from egoist.ext.serverprocess.lazyparams import find_free_port, create_sentinel_file

    app.include("egoist.ext.serverprocess")  # for add_server_process

    app.add_server_process(
        "gofmtrpc -addr :{port} -sentinel {sentinel}",
        params=dict(port=find_free_port, sentinel=create_sentinel_file),
        name=NAME,
    )
