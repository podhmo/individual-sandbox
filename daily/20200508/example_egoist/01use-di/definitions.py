import typing as t
from egoist.app import App, SettingsDict


settings: SettingsDict = {"root": "", "here": __file__}
app = App(settings)

app.include("egoist.directives.define_cli")
app.include("egoist.directives.define_struct_set")


@app.define_cli("egoist.generate.clikit:walk")
def cmd__hello(*, name: str) -> None:
    """hello message"""
    from egoist.generate.clikit import runtime, clikit

    with runtime.generate(clikit):
        runtime.printf("hello %s\n", name)


class X:
    name: str


class Y:
    name: str


class Z:
    x: X
    y: t.Optional[X]


@app.define_struct_set("egoist.generate.structkit:walk")
def internal__components() -> None:
    from egoist.generate.structkit import runtime, structkit

    with runtime.generate(structkit, classes=[Z]) as m:
        m.package("components")


if __name__ == "__main__":
    app.run()
