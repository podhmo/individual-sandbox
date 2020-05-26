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


XorY = NewNamedType("XorY", t.Union[X, Y])
XList = NewNamedType("XList", t.List[X])
XListOrYList = NewNamedType("XListOrYList", t.Union[XList, t.List[Y]])


class W:
    xy_union_list: t.List[XorY]
    xy_list_union: XListOrYList


classes = [W, XorY, XListOrYList]  # todo: omit XorY


@app.define_struct_set("egoist.generators.structkit:walk")
def model__objects() -> None:
    from egoist.generators.structkit import runtime, structkit

    with runtime.generate(structkit, classes=classes) as m:
        m.package("model")


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
