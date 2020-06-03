from egoist.app import create_app, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "output", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_file")
app.include("discovery")
app.include("ok_service")


@app.define_file("egoist.generators.filekit:walk", suffix=".json")
def ok() -> None:
    import requests
    from egoist.generators.filekit import runtime
    from discovery import get_discovery

    with runtime.create_file() as wf:
        url = get_discovery().lookup("HELLO")
        res = requests.get(url)
        res.raise_for_status()
        print(res.json(), file=wf)


@app.define_file("egoist.generators.filekit:walk", suffix=".json")
def ok2() -> None:
    import requests
    from egoist.generators.filekit import runtime
    from discovery import get_discovery

    with runtime.create_file() as wf:
        url = get_discovery().lookup("HELLO")
        res = requests.get(url)
        res.raise_for_status()
        print(res.json(), file=wf)


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
