from __future__ import annotations
from egoist.app import create_app, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_file")

app.include("gofmtrpc")
app.include("components")


@app.define_file("egoist.generators.filekit:walk")
def noop() -> None:
    from egoist.generators.filekit import runtime

    with runtime.create_file() as wf:
        print("noop", file=wf)


@app.define_file("egoist.generators.filekit:walk", suffix=".formatted.go")
def hello__hello(*, filename: str = "./hello/hello.go") -> None:
    from egoist.generators.filekit import runtime
    from discovery import get_discovery
    from components import get_file_opener, get_http_client

    with runtime.create_file() as wf:
        with get_file_opener().open(filename) as rf:
            code = rf.read()

        url = get_discovery().lookup("gofmtrpc")
        res = get_http_client().post(
            url,
            json={
                "jsonrpc": "2.0",
                "id": 1,
                "method": "format",
                "params": {"code": code},
            },
        )
        res.raise_for_status()
        print(res.json()["result"], file=wf)


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
