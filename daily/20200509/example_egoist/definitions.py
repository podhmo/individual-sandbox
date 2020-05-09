from __future__ import annotations
from egoist.app import App, SettingsDict

settings: SettingsDict = {"root": "", "here": __file__}
app = App(settings)

app.include("egoist.directives.define_cli")
app.include("egoist.directives.define_struct_set")


class Article:
    title: str
    content: str


@app.define_struct_set("egoist.generators.structkit:walk")
def model__objects() -> None:
    from egoist.generators.structkit import runtime, structkit
    with runtime.generate(structkit, classes=[Article]) as m:
        m.package("model")


@app.define_cli("egoist.generators.clikit:walk")
def cmd__hello(*, name: str, content: str = "") -> None:
    from egoist.generators.clikit import runtime, clikit

    with runtime.generate(clikit) as m:
        runtime.printf("hello")


@app.define_cli("egoist.generators.clikit:walk")
def cmd__load(*, filename: str) -> None:
    from egoist.generators.clikit import runtime, clikit

    with runtime.generate(clikit) as m:
        runtime.printf("load")


if __name__ == "__main__":
    app.run()
