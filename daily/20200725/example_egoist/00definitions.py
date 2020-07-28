import typing_extensions as tx
from egoist.app import create_app, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "cmd/", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_cli")


@app.define_cli("egoist.generators.clikit:walk")
def hello(*, name: tx.Annotated[str, Description("greeting target name")]) -> None:
    """hello message"""
    from egoist.generators.clikit import runtime, clikit

    with runtime.generate(clikit):
        runtime.printf("hello %s\n", name)


@app.define_cli("egoist.generators.clikit:walk")
def byebye(*, name: str) -> None:
    """byebye message"""
    from egoist.generators.clikit import runtime, clikit

    with runtime.generate(clikit):
        runtime.printf("byebye %s\n", name)


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
