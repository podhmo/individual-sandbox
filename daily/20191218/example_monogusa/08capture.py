import typing as t


def bind_parser(parser, *, rec: t.Callable[[t.Any], t.Any]):
    rec(parser.add_argument("--foo", action="store_true"))
    rec(parser.add_argument("--bar", action="store_true"))


def use_normal():
    import argparse

    parser = argparse.ArgumentParser(prog="app.py")
    bind_parser(parser, rec=id)
    print(parser.format_help())


def use_gencode():
    from codeobject import Module, codeobject

    @codeobject
    def ArgumentParser(m: Module, name: str) -> Module:
        pass

    m = Module()
    parser = m.let("parser", ArgumentParser(prog="app.py"))
    bind_parser(parser, rec=m.stmt)
    print(m)


use_normal()
use_gencode()
