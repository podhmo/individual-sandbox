from egoist.app import create_app, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_file")
app.include("discovery")
app.include("gofmt_service")


@app.define_file("egoist.generators.filekit:walk", suffix=".formatted.go")
def hello__hello(*, filename: str = "./hello/hello.go") -> None:
    import requests
    from egoist.generators.filekit import runtime
    from discovery import get_discovery

    with runtime.create_file() as wf:
        url = get_discovery().lookup("gofmtrpc")
        with open(filename) as rf:
            code = rf.read()

        res = requests.post(
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
