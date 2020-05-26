from __future__ import annotations
import typing as t
from egoist.app import create_app, SettingsDict, parse_args
from egoist.typing import NewNamedType

settings: SettingsDict = {"rootdir": "", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_struct_set")


class X:
    name: str


class Y:
    name: str
    memo: str


class W:
    xy_union_list: t.List[t.Union[X, Y]]
    xy_list_union: t.Union[t.List[X], t.List[Y]]


classes = [W, t.Union[X, Y], t.Union[t.List[X], t.List[Y]]]


@app.define_struct_set("egoist.generators.structkit:walk")
def model__objects() -> None:
    from egoist.generators.structkit import runtime, structkit

    with runtime.generate(structkit, classes=classes) as m:
        m.package("model")


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
