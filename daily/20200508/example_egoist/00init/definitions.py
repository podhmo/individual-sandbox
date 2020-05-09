from egoist.app import App, SettingsDict

settings: SettingsDict = {"root": "cmd/", "here": __file__}
app = App(settings)

app.include("egoist.directives.define_cli")


@app.define_cli("egoist.generate.clikit:walk")
def hello(*, name: str) -> None:
    """hello message"""
    from egoist.generate.clikit import runtime, clikit

    with runtime.generate(clikit):
        runtime.printf("hello %s\n", name)


if __name__ == "__main__":
    app.run()
