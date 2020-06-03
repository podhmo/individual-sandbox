import sys
from egoist.app import create_app, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "output", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_file")
app.include("discovery")


@app.define_file("egoist.generators.filekit:walk", suffix=".json")
def ok() -> None:
    import pathlib
    from egoist.generators.filekit import runtime
    from egoist.internal.netutil import find_free_port
    from discovery import get_discovery
    import util

    sentinel = util.create_sentinel_file()
    server_py = pathlib.Path(__file__).absolute().with_name("server.py")
    port = find_free_port()

    get_discovery().register("HELLO", url=f"http://127.0.0.1:{port}")

    argv = [
        sys.executable,
        server_py,
        "--port",
        str(port),
        "--host",
        "127.0.0.1",
        "--sentinel",
        sentinel,
    ]

    with util.ConnectedProcess().spawn(argv, sentinel=sentinel):
        import requests

        url = get_discovery().lookup("HELLO")
        res = requests.get(url)
        res.raise_for_status()

        with runtime.create_file() as wf:
            print(res.json(), file=wf)


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
