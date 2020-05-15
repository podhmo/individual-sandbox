from __future__ import annotations
import typing as t
from egoist.app import App, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "", "here": __file__}
app = App(settings)

app.include("egoist.directives.define_struct_set")
app.include("egoist.directives.define_cli")


class Author:
    name: str
    # createdAt: datetime.date


class Article:
    title: str
    author: t.Optional[Author]
    content: str
    comments: t.List[Comment]


class Comment:
    author: Author
    content: str


@app.define_struct_set("egoist.generators.structkit:walk")
def model__objects() -> None:
    from egoist.generators.structkit import runtime, structkit

    with runtime.generate(structkit, classes=[Article]) as m:
        m.package("model")


# cli
@app.define_cli("egoist.generators.clikit:walk")
def cmd__load(*, filename: str) -> None:
    from egoist.generators.clikit import runtime, clikit

    with runtime.generate(clikit) as m:
        os_pkg = m.import_("os")
        load_pkg = m.import_("m/load")
        model_pkg = m.import_("m/model")

        f = m.symbol("f")
        ob = m.symbol("ob")
        err = m.symbol("err")

        m.stmt(f"{f}, {err} := {os_pkg.Open(filename)}")
        m.stmt(f"if {err} != nil {{")
        with m.scope():
            m.return_(err)
        m.stmt("}")

        m.stmt(f"defer {f}.Close()")
        m.stmt(f"var {ob} {model_pkg.Article}")
        p = m.let("p", f"&{ob}")
        m.return_(load_pkg.LoadAndPrint(f, p))


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
