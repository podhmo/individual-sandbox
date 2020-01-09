from __future__ import annotations
import typing as t
from handofcats.injector import Injector
from _codeobject import Module, Symbol


def hello(*, name: str = "world") -> None:
    """-- hello world --"""
    print(f"hello {name}")


def byebye(name: str) -> None:
    print(f"byebye {name}")


class MultiDriver:
    def __init__(self):
        self.functions = []

    def register(self, fn: t.Callable[..., t.Any]) -> None:
        self.functions.append(fn)

    def run(self, m: M, parser: t.Any, argv=None, *, ignore_logging=False):
        subparsers = m.let("subparsers", parser.add_subparsers())
        m.setattr(subparsers, "required", True)  # xxx:
        m.sep()

        for original_fn in self.functions:
            fn = m.let("fn", m.symbol(original_fn))
            sub_parser = m.let(
                "sub_parser",
                subparsers.add_parser(
                    m.getattr(fn, "__name__"), help=m.getattr(fn, "__doc__")
                ),
            )
            Injector(original_fn).inject(sub_parser, callback=m.stmt)
            m.stmt(sub_parser.set_defaults(sub_command=fn))
            m.sep()

        args = parser.parse_args(argv)
        return m.return_(args)


class M:
    def let(self, name: str, ob: t.Any) -> t.Any:
        return ob

    def stmt(self, ob: t.Any) -> t.Any:
        return ob

    def sep(self):
        return None

    def return_(self, ob: t.Any) -> t.Any:
        return ob

    def symbol(self, ob: t.Any) -> t.Any:
        return ob

    def setattr(self, ob: t.Any, name: str, val: t.Any) -> None:
        setattr(ob, name, val)

    def getattr(self, ob: t.Any, name: str) -> t.Optional[t.Any]:
        return getattr(ob, name)


md = MultiDriver()
md.register(hello)
md.register(byebye)

# run
m = M()
from argparse import ArgumentParser  # noqa

parser = ArgumentParser()
print(md.run(m, parser, ["hello", "--name", "world"]))
print("----------------------------------------")
# codegen
m = Module()
with m.def_("main", "argv: t.Optional = None"):
    argparse = m.import_("argparse")
    parser = m.let("parser", argparse.ArgumentParser())
    md.run(m, parser, Symbol("argv"))
print(m)
