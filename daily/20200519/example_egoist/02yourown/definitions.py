from egoist.app import App, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "cmd/", "here": __file__}
app = App(settings)

app.include("egoist.commands.generate")
app.include("commands.now")
app.include("egoist.directives.define_cli")


@app.define_cli("egoist.generators.clikit:walk")
def hello(*, name: str) -> None:
    """hello message"""
    from egoist.generators.clikit import runtime, clikit

    with runtime.generate(clikit):
        runtime.printf("hello %s\n", name)


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
