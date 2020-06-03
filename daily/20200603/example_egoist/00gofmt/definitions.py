from __future__ import annotations
from egoist.app import create_app, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_struct_set")


@app.define_struct_set("egoist.generators.structkit:walk")
def model__objects() -> None:
    from egoist.generators.structkit import runtime, structkit
    from objects import Article

    with runtime.generate(structkit, classes=[Article]) as m:
        m.package("model")


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
