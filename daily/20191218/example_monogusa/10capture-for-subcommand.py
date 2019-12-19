from prestring.utils import UnRepr
from handofcats.injector import Injector


def hello(*, name: str, age: int, debug: bool = False) -> None:
    """hello world"""
    pass


def byebye(name: str) -> None:
    pass


def use_normal():
    import argparse

    parser = argparse.ArgumentParser(prog="app.py")
    subparsers = parser.add_subparsers(
        title="subcommands", required=True, dest="subcommand"
    )

    r = []
    for fn in [hello, byebye]:
        sub_parser = subparsers.add_parser(fn.__name__, help=fn.__doc__)
        Injector(fn).inject(sub_parser, do=id)
        r.append(sub_parser)

    print(parser.format_help())
    for sp in r:
        print(sp.format_help())


def use_gencode():
    from codeobject import Module

    m = Module()

    argparse = m.import_("argparse")
    with m.def_("create_parser"):
        parser = m.let("parser", argparse.ArgumentParser(prog="app.py"))
        subparsers = m.let(
            "subparsers",
            parser.add_subparsers(
                title="subcommands", required=True, dest="subcommand"
            ),
        )
        m.sep()
        for fn in [hello, byebye]:
            m.stmt(f"# for {fn.__name__}")
            sub_parser = m.let(
                "sub_parser",
                subparsers.add_parser(
                    fn.__name__, help=UnRepr(f"{fn.__name__}.__doc__")
                ),
            )
            Injector(fn).inject(sub_parser, callback=m.stmt)
            m.stmt(sub_parser.set_defaults(subcommand=fn))
            m.sep()
        m.return_(parser)

    print(m)


use_normal()
use_gencode()
