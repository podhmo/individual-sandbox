from egoist.app import App, SettingsDict

settings: SettingsDict = {"rootdir": "cmd/", "here": __file__}
app = App(settings)

app.include("egoist.directives.define_cli")


@app.define_cli("egoist.generators.clikit:walk")
def hello(*, name: str, age: int = 20, who: str = "foo") -> None:
    """hello message"""
    from egoist.generators.clikit import runtime, clikit

    options = runtime.get_cli_options()
    options.name.help = "the name of target person"
    options.age.help = "age of subject"
    options.who.help = "name of subject"

    with runtime.generate(clikit):
        runtime.printf("%s(%d): hello %s\n", who, age, name)


if __name__ == "__main__":
    app.run()
