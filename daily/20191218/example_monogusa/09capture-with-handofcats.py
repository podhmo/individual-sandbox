from handofcats.injector import Injector


def hello(*, name: str, age: int, debug: bool = False) -> None:
    pass


def use_normal():
    import argparse

    parser = argparse.ArgumentParser(prog="app.py")
    Injector(hello).inject(parser, callback=id)
    print(parser.format_help())


def use_gencode():
    from codeobject import Module, codeobject

    @codeobject
    def ArgumentParser(m: Module, name: str) -> Module:
        pass

    m = Module()
    parser = m.let("parser", ArgumentParser(prog="app.py"))
    Injector(hello).inject(parser, callback=m.stmt)
    print(m)


use_normal()
use_gencode()
