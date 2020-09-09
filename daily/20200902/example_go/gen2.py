import typing as t
import itertools
from prestring.go.codeobject import Module


def visit(m: Module, *, names: t.List[str]) -> None:
    m.stmt("func run() error {")
    with m.scope():
        m.stmt(f"return {names[0]}()")
    m.stmt("}")

    for name, prev in itertools.zip_longest(names, names[1:]):
        if prev is None:
            visit_core(m, name=name)
        else:
            visit_wrap(m, name=name, prev=prev)


def visit_core(m: Module, *, name: str) -> None:
    m.stmt(f"func {name}() error {{")
    with m.scope():
        m.stmt('return fmt.Errorf("xxx")')
    m.stmt("}")


def visit_wrap(m: Module, *, name: str, prev: str) -> None:
    m.stmt(f"func {name}() error {{")
    with m.scope():
        m.stmt(f'return errors.Wrap({prev}(), "on {name}")')
    m.stmt("}")


def gen(*, m=None):
    m = m or Module()

    m.stmt("package main")
    m.sep()
    m.stmt("import (")
    with m.scope():
        m.stmt('"fmt"')
        m.stmt('"log"')
        m.sep()
        m.stmt('"github.com/pkg/errors"')
    m.stmt(")")
    m.sep()
    m.stmt("func main() {")
    with m.scope():
        m.stmt("if err := run(); err != nil {")
        with m.scope():
            m.stmt('log.Fatalf("!!%+v", err)')
        m.stmt("}")
    m.stmt("}")
    m.sep()

    visit(m, names=["f", "g"])
    return m


if __name__ == "__main__":
    m = gen()
    print(m)
